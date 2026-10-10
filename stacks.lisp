;;;; stacks.lisp
;;;; Assigning data stacks to a lowered program, after Goose's recipe in spec
;;;; section 10.3.

(in-package #:sheaf)

(defvar *lowered*)
(defvar *storage*)
(defvar *clones*)
(defvar *defs*)
(defvar *cycles*)
(defvar *globals*)
(defvar *global-busy*)
(defvar *stack-count*)
(defvar *summaries*)
(defvar *placements*)
(defvar *banked*)

;;; Storage

(defun size-class (type)
  "TYPE's size class: `:fixed`, `:variable` or `:resizable`."
  (or (third (assoc (ir-type-name type) *prelude-types*)) :fixed))

(defun field-types (type)
  (loop :for constructor :in (lowered-constructors *lowered* type)
        :append (ir-constructor-fields constructor)))

(defun pooled-p (type)
  "Whether TYPE is a data type that contains itself, so its values are cells in a pool."
  (and (eq (ir-type-kind type) :data)
       (let ((seen '()) (pending (field-types type)))
         (loop :while pending
               :do (let ((next (pop pending)))
                     (when (eq next type) (return t))
                     (unless (member next seen)
                       (push next seen)
                       (when (eq (ir-type-kind next) :data)
                         (setf pending (append (field-types next) pending)))))))))

(defun holds-storage-p (type)
  "Whether a value of TYPE is, or holds, a pooled cell or a resizable value."
  (multiple-value-bind (known found) (gethash type *storage*)
    (if found
        known
        (setf (gethash type *storage*)
              (or (eq (size-class type) :resizable)
                  (member (ir-type-kind type) '(:storage :view))
                  (pooled-p type)
                  (and (some #'holds-storage-p
                             (ecase (ir-type-kind type)
                               (:data (field-types type))
                               ((:scalar :storage :view) '())))
                       t))))))

(defun resizable-node-p (node)
  (eq (size-class (ir-node-type node)) :resizable))

;;; Where a value goes

(defun pattern-bindings (pattern)
  "The names PATTERN binds, each with its type."
  (etypecase pattern
    ((or ir-pwild ir-plit) '())
    (ir-pvar (list (cons (ir-pvar-name pattern) (ir-pattern-type pattern))))
    (ir-pas (cons (cons (ir-pas-name pattern) (ir-pattern-type pattern))
                  (pattern-bindings (ir-pas-pattern pattern))))
    (ir-pcon (mapcan #'pattern-bindings (ir-pcon-patterns pattern)))))

(defun fresh-p (name)
  "Whether the result of the def NAME holds storage it builds, which goes in the destination."
  (and (member :fresh (root-summary-result (gethash name *summaries*))) t))

(defun reached-table (def)
  "A set of the names in DEF whose storage its result holds."
  (let ((reached (make-hash-table :test 'eq)))
    (dolist (name (root-summary-reached (gethash (ir-def-name def) *summaries*)))
      (setf (gethash name reached) t))
    reached))

(defun local-site-p (def reached)
  "Whether DEF builds ordinary storage that does not reach its result, which a
pool of its own holds."
  (labels ((scan (node reaches)
             (etypecase node
               ((or ir-atom ir-foreign) nil)
               (ir-prim (and (not reaches) (primitive-allocates (ir-prim-op node))
                             (not (resizable-node-p node))))
               (ir-let (or (scan (ir-let-value node) (gethash (ir-let-name node) reached))
                           (scan (ir-let-body node) reaches)))
               (ir-construct (and (not reaches) (pooled-p (ir-node-type node))))
               (ir-call (and (not reaches) (placement-ordinary (callee-placement node))))
               (ir-match (some (lambda (branch) (scan (ir-branch-body branch) reaches))
                               (ir-match-branches node))))))
    (scan (ir-def-body def) (fresh-p (ir-def-name def)))))

(defun builds-storage-p (def)
  "Whether DEF builds cells or arrays, which a region holds. Growable values go
on stacks of their own."
  (block found
    (walk-ir (lambda (node)
               (when (typecase node
                       (ir-construct (pooled-p (ir-node-type node)))
                       (ir-call (placement-ordinary (callee-placement node)))
                       (ir-prim (and (primitive-allocates (ir-prim-op node))
                                     (not (resizable-node-p node)))))
                 (return-from found t)))
             (ir-def-body def))
    nil))

;;; Placements

(defstruct (placement (:constructor make-placement (ordinary growables banked)))
  "What a def's result needs from its caller: whether an ordinary destination
for cells and arrays, how many stacks for growable values, one each, and
whether instead a bank, which hands out a stack for each growable value as the
run builds it."
  (ordinary nil) (growables 0 :type fixnum) (banked nil))

(defparameter *no-placement* (make-placement nil 0 nil))

(defun callee-placement (call)
  (gethash (ir-call-function call) *placements* *no-placement*))

(defun growable-site-p (node)
  "Whether NODE builds a growable value."
  (and (typep node '(or ir-prim ir-foreign))
       (resizable-node-p node)
       (or (typep node 'ir-foreign) (primitive-allocates (ir-prim-op node)))))

(defun needs (node reaches reached)
  "Whether NODE builds ordinary storage that reaches the result, how many
stacks it needs for growable values, and whether it calls a def that needs a
bank, when REACHES says its own result does. REACHED holds the names of the
lets whose values reach the result."
  (etypecase node
    (ir-atom (values nil 0 nil))
    (ir-foreign (values nil (if (and reaches (growable-site-p node)) 1 0) nil))
    (ir-construct (values (and reaches (pooled-p (ir-node-type node))) 0 nil))
    (ir-prim (cond ((not (primitive-allocates (ir-prim-op node))) (values nil 0 nil))
                   ((growable-site-p node) (values nil (if reaches 1 0) nil))
                   (t (values (and reaches t) 0 nil))))
    (ir-call (let ((placement (callee-placement node)))
               (values (and reaches (placement-ordinary placement))
                       (if reaches (placement-growables placement) 0)
                       (and reaches (placement-banked placement)))))
    (ir-let (multiple-value-bind (ordinary growables banked)
                (needs (ir-let-value node) (gethash (ir-let-name node) reached) reached)
              (multiple-value-bind (body-ordinary body-growables body-banked)
                  (needs (ir-let-body node) reaches reached)
                (values (or ordinary body-ordinary) (+ growables body-growables) (or banked body-banked)))))
    (ir-match (let ((ordinary nil) (growables 0) (banked nil))
                (dolist (branch (ir-match-branches node) (values ordinary growables banked))
                  (multiple-value-bind (branch-ordinary branch-growables branch-banked)
                      (needs (ir-branch-body branch) reaches reached)
                    (setf ordinary (or ordinary branch-ordinary)
                          growables (max growables branch-growables)
                          banked (or banked branch-banked))))))))

(defun returns-own-name-p (let-node)
  "Whether LET-NODE ends by returning the value it binds."
  (loop :for body := (ir-let-body let-node) :then (ir-let-body body)
        :unless (typep body 'ir-let)
          :return (and (typep body 'ir-local) (eq (ir-local-name body) (ir-let-name let-node)))))

(defun tail-need (node)
  "How many stacks the growable values NODE returns need."
  (etypecase node
    ((or ir-atom ir-construct) 0)
    ((or ir-prim ir-foreign) (if (growable-site-p node) 1 0))
    (ir-call (placement-growables (callee-placement node)))
    (ir-let (if (returns-own-name-p node)
                (tail-need (ir-let-value node))
                (tail-need (ir-let-body node))))
    (ir-match (reduce #'max (ir-match-branches node) :key (lambda (branch) (tail-need (ir-branch-body branch)))
                                                      :initial-value 0))))

(defun tail-banked-p (node)
  "Whether the growable values NODE returns need a bank."
  (etypecase node
    ((or ir-atom ir-construct ir-prim ir-foreign) nil)
    (ir-call (placement-banked (callee-placement node)))
    (ir-let (tail-banked-p (if (returns-own-name-p node) (ir-let-value node) (ir-let-body node))))
    (ir-match (some (lambda (branch) (tail-banked-p (ir-branch-body branch)))
                    (ir-match-branches node)))))

(defun def-placement (def)
  (multiple-value-bind (ordinary growables banked)
      (needs (ir-def-body def) (fresh-p (ir-def-name def)) (reached-table def))
    (if (or banked (gethash (ir-def-name def) *banked*))
        (make-placement ordinary 0 t)
        (make-placement ordinary growables nil))))

(defun cycle-bound (members defs)
  "The most stacks the growable values of the defs MEMBERS can need unless a
growable value is built at every level of their recursion."
  (let ((total 0))
    (dolist (name members total)
      (walk-ir (lambda (node)
                 (incf total (cond ((growable-site-p node) 1)
                                   ((and (typep node 'ir-call) (not (member (ir-call-function node) members)))
                                    (placement-growables (callee-placement node)))
                                   (t 0))))
               (ir-def-body (gethash name defs))))))

(defun compute-placements (defs)
  "A table from the name of each def in DEFS to its `placement`. A cycle that
builds a growable value at every level of its recursion needs a bank."
  (let ((*placements* (make-hash-table :test 'eq))
        (*banked* (make-hash-table :test 'eq)))
    (dolist (members (strongly-connected-components
                      (loop :for name :being :the :hash-keys :of defs :collect name)
                      (lambda (name) (callees (gethash name defs)))))
      (let ((bound (cycle-bound members defs)))
        (dolist (name members)
          (setf (gethash name *placements*) *no-placement*))
        (loop :for changed := nil
              :do (dolist (name members)
                    (let ((new (def-placement (gethash name defs))))
                      (when (> (placement-growables new) bound)
                        (dolist (member members)
                          (setf (gethash member *banked*) t))
                        (setf changed t))
                      (unless (equalp new (gethash name *placements*))
                        (setf changed t
                              (gethash name *placements*) new))))
              :while changed)))
    *placements*))

;;; Recursive cycles

(defun callees (def)
  (let ((names '()))
    (walk-ir (lambda (node) (when (typep node 'ir-call) (pushnew (ir-call-function node) names)))
             (ir-def-body def))
    names))

(defun find-cycles (defs)
  "A table from the name of each def in a recursive cycle to a cons of the
cycle's members and whether they build storage, which needs a region."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (scc (strongly-connected-components
                  (loop :for name :being :the :hash-keys :of defs :collect name)
                  (lambda (name) (callees (gethash name defs)))))
      (when (or (rest scc) (member (first scc) (callees (gethash (first scc) defs))))
        (let ((cycle (cons scc (and (some (lambda (name) (builds-storage-p (gethash name defs))) scc)
                                    t))))
          (dolist (name scc)
            (setf (gethash name table) cycle)))))
    table))

;;; Assignment

(defun take-index (busy)
  "The lowest stack index not in BUSY."
  (let ((index (loop :for i :from 0 :unless (member i busy) :return i)))
    (setf *stack-count* (max *stack-count* (1+ index)))
    index))

(defun with-index (busy index)
  (if (or (null index) (member index busy))
      busy
      (sort (list* index (copy-list busy)) #'<)))

(defun take-stacks (count busy)
  "COUNT stack indices not in BUSY, each different."
  (let ((stacks '()))
    (dotimes (i count (nreverse stacks))
      (push (take-index (reduce #'with-index stacks :initial-value busy)) stacks))))

(defun clone-def (name destination growables bank region busy)
  "The name of the clone of the def NAME for a context: the stack its result is
built on, the stacks its growable values are built on or the bank that hands
them out, the region its cycle allocates in, and the stacks in use on entry."
  (let ((key (list name destination growables bank region busy)))
    (or (gethash key *clones*)
        (let ((clone (make-symbol (format nil "~a[~@[dest ~d ~]~@[grow~{ ~d~} ~]~@[bank ~d ~]~@[region ~d ~]busy~{ ~d~}]"
                                          (symbol-name name) destination growables bank region busy))))
          (setf (gethash key *clones*) clone)
          (setf (gethash clone *defs*)
                (assign-def (gethash name (lowered-defs *lowered*)) clone destination growables bank region busy))
          clone))))

(defun global-clone (name)
  (destructuring-bind (&optional destination bank &rest growables) (gethash name *globals*)
    (clone-def name destination growables bank nil *global-busy*)))

(defun assign-def (def clone destination growables bank region entry-busy)
  (let* ((cycle (gethash (ir-def-name def) *cycles*))
         (reached (reached-table def))
         (pool (if cycle
                   region
                   (and (local-site-p def reached) (take-index entry-busy))))
         (new (copy-ir-def def)))
    (labels ((storage-stack (reaches)
               (or (if reaches destination pool)
                   (error "~s has no stack to build ~:[a local~;its result~] in"
                          (ir-def-name def) reaches)))
             (growable-stacks (count reaches owned supply)
               (let ((stacks (if reaches supply owned)))
                 (when (< (length stacks) count)
                   (error "~s has no stacks to build ~d growable value~:p in" (ir-def-name def) count))
                 (subseq stacks 0 count)))
             (rewrite-atom (node)
               (if (typep node 'ir-global)
                   (let ((copy (copy-ir-global node)))
                     (setf (ir-global-name copy) (global-clone (ir-global-name node)))
                     copy)
                   node))
             (rewrite-call (node reaches busy owned supply owned-bank)
               (let* ((callee (ir-call-function node))
                      (callee-cycle (gethash callee *cycles*))
                      (placement (callee-placement node))
                      (dest (and (placement-ordinary placement) (storage-stack reaches)))
                      (call-bank (and (or (placement-banked placement)
                                          (and bank reaches (plusp (placement-growables placement))))
                                      (or (if reaches bank owned-bank)
                                          (error "~s has no bank to build a growable result in"
                                                 (ir-def-name def)))))
                      (stacks (if call-bank
                                  '()
                                  (growable-stacks (placement-growables placement) reaches owned supply)))
                      (copy (copy-ir-call node)))
                 (setf (ir-call-args copy) (mapcar #'rewrite-atom (ir-call-args node))
                       (ir-call-destination copy) dest
                       (ir-call-growables copy) stacks
                       (ir-call-bank copy) call-bank
                       (ir-call-function copy)
                       (cond ((and callee-cycle (eq callee-cycle cycle))
                              (when (set-difference busy entry-busy)
                                (error "~s keeps a growable local across a call back into its recursive cycle: https://github.com/takeiteasy/sheaf/issues/17"
                                       (ir-def-name def)))
                              (clone-def callee dest stacks call-bank region entry-busy))
                             (callee-cycle
                              (let* ((own-region (and (null dest) (cdr callee-cycle) (take-index busy)))
                                     (callee-region (or dest own-region)))
                                (setf (ir-call-region copy) own-region)
                                (clone-def callee dest stacks call-bank callee-region (with-index busy callee-region))))
                             (t (clone-def callee dest stacks call-bank nil busy))))
                 copy))
             (rewrite (node reaches busy owned supply owned-bank)
               (etypecase node
                 ((or ir-literal ir-local ir-foreign) node)
                 (ir-global (rewrite-atom node))
                 (ir-let
                  (let* ((value (ir-let-value node))
                         (reaches-value (gethash (ir-let-name node) reached))
                         (returned (and (not reaches-value) (returns-own-name-p node)))
                         (stacks (unless (or reaches-value returned) (take-stacks (tail-need value) busy)))
                         (let-bank (and (not reaches-value) (not returned) (tail-banked-p value)
                                        (take-index (reduce #'with-index stacks :initial-value busy))))
                         (inner (with-index (reduce #'with-index stacks :initial-value busy) let-bank))
                         (used (nth-value 1 (needs value reaches-value reached)))
                         (copy (copy-ir-let node)))
                    (setf (ir-let-stacks copy) stacks
                          (ir-let-bank copy) let-bank
                          (ir-let-value copy) (rewrite value reaches-value inner (if returned owned stacks) supply
                                                       (if returned owned-bank let-bank))
                          (ir-let-body copy) (rewrite (ir-let-body node) reaches inner owned (nthcdr used supply)
                                                      owned-bank))
                    copy))
                 (ir-call (rewrite-call node reaches busy owned supply owned-bank))
                 (ir-construct
                  (let ((copy (copy-ir-construct node)))
                    (setf (ir-construct-args copy) (mapcar #'rewrite-atom (ir-construct-args node)))
                    (when (pooled-p (ir-node-type node))
                      (setf (ir-construct-stack copy) (storage-stack reaches)))
                    copy))
                 (ir-prim
                  (let ((copy (copy-ir-prim node)))
                    (setf (ir-prim-args copy) (mapcar #'rewrite-atom (ir-prim-args node)))
                    (when (primitive-allocates (ir-prim-op node))
                      (cond ((not (resizable-node-p node))
                             (setf (ir-prim-stack copy) (storage-stack reaches)))
                            ((and bank reaches)
                             (setf (ir-prim-bank copy) bank))
                            (t
                             (setf (ir-prim-stack copy) (first (growable-stacks 1 reaches owned supply))))))
                    copy))
                 (ir-match
                  (let ((copy (copy-ir-match node)))
                    (setf (ir-match-scrutinee copy) (rewrite-atom (ir-match-scrutinee node))
                          (ir-match-branches copy)
                          (loop :for branch :in (ir-match-branches node)
                                :collect (let ((branch-copy (copy-ir-branch branch)))
                                           (setf (ir-branch-body branch-copy)
                                                 (rewrite (ir-branch-body branch) reaches busy owned supply owned-bank))
                                           branch-copy)))
                    copy)))))
      (setf (ir-def-name new) clone
            (ir-def-source new) (ir-def-name def)
            (ir-def-destination new) destination
            (ir-def-growables new) growables
            (ir-def-bank new) bank
            (ir-def-pool new) (and (not cycle) pool)
            (ir-def-body new) (rewrite (ir-def-body def)
                                       (fresh-p (ir-def-name def))
                                       (with-index entry-busy pool)
                                       '()
                                       growables
                                       '()))
      new)))

(defun check-distinct (name indices)
  (let ((used (remove nil indices)))
    (unless (= (length used) (length (remove-duplicates used)))
      (error "~s: stack ~{~d~^ ~} is used twice by one result" name used))))

(defun check-assigned (lowered)
  "Signal an error unless every cell and storage-holding call in LOWERED has a
stack below its stack count. Returns T."
  (let ((*lowered* lowered)
        (*storage* (make-hash-table :test 'eq))
        (count (lowered-stack-count lowered)))
    (unless count
      (error "The program has no stack assignment"))
    (maphash
     (lambda (name def)
       (flet ((check-index (index what)
                (unless (and (integerp index) (< -1 index count))
                  (error "~s: ~a has no stack below ~d" name what count))))
         (walk-ir (lambda (node)
                    (typecase node
                      (ir-construct (when (pooled-p (ir-node-type node))
                                      (check-index (ir-construct-stack node) "a cell")))
                      (ir-call (let ((callee (gethash (ir-call-function node) (lowered-defs lowered))))
                                 (when (ir-def-destination callee)
                                   (check-index (ir-call-destination node) "a call's result"))
                                 (unless (= (length (ir-call-growables node)) (length (ir-def-growables callee)))
                                   (error "~s: a call to ~s has ~d growable stacks, not ~d" name
                                          (ir-call-function node) (length (ir-call-growables node))
                                          (length (ir-def-growables callee))))
                                 (dolist (index (ir-call-growables node))
                                   (check-index index "a call's growable result"))
                                 (when (ir-call-bank node)
                                   (check-index (ir-call-bank node) "a call's bank"))
                                 (check-distinct name (list* (ir-call-destination node) (ir-call-bank node)
                                                             (ir-call-growables node))))
                               (when (ir-call-region node)
                                 (check-index (ir-call-region node) "a call's region")))
                      (ir-prim (cond ((ir-prim-bank node)
                                      (unless (eql (ir-prim-bank node) (ir-def-bank def))
                                        (error "~s: a growable value is built in a bank the def does not have" name)))
                                     ((primitive-allocates (ir-prim-op node))
                                      (check-index (ir-prim-stack node) "an array"))))
                      (ir-let (dolist (index (ir-let-stacks node))
                                (check-index index "a growable local"))
                              (when (ir-let-bank node)
                                (check-index (ir-let-bank node) "a bank"))
                              (check-distinct name (cons (ir-let-bank node) (ir-let-stacks node))))))
                  (ir-def-body def))
         (dolist (index (ir-def-growables def))
           (check-index index "a growable result"))
         (when (ir-def-bank def)
           (check-index (ir-def-bank def) "a bank"))
         (check-distinct name (list* (ir-def-destination def) (ir-def-bank def) (ir-def-growables def)))
         (when (ir-def-pool def)
           (check-index (ir-def-pool def) "a pool"))))
     (lowered-defs lowered))
    t))

(defun assign-stacks (lowered)
  "LOWERED with a data stack assigned to every cell and growable value, after
Goose's recipe (spec section 10.3). Each def is cloned for the context it is
called in, so every stack index is a constant. Returns the new program, whose
`lowered-stack-count` is the number of stacks it needs."
  (check-lowered lowered)
  (let* ((defs (lowered-defs lowered))
         (*lowered* lowered)
         (*storage* (make-hash-table :test 'eq))
         (*clones* (make-hash-table :test 'equal))
         (*defs* (make-hash-table :test 'eq))
         (*summaries* (root-summaries lowered))
         (*placements* (compute-placements defs))
         (*cycles* (find-cycles defs))
         (*globals* (make-hash-table :test 'eq))
         (*global-busy* '())
         (*stack-count* 0)
         (root (gethash (lowered-root lowered) defs)))
    (flet ((take-global (name)
             (let ((placement (gethash name *placements*)))
               (flet ((take ()
                        (let ((index (take-index *global-busy*)))
                          (setf *global-busy* (with-index *global-busy* index))
                          index)))
                 (setf (gethash name *globals*)
                       (let* ((destination (and (placement-ordinary placement) (take)))
                              (bank (and (placement-banked placement) (take))))
                         (list* destination bank
                                (loop :repeat (placement-growables placement) :collect (take)))))))))
      (maphash (lambda (name def)
                 (when (eq (ir-def-kind def) :value)
                   (take-global name)))
               defs)
      (when (eq (ir-def-kind root) :function)
        (take-global (lowered-root lowered))))
    (maphash (lambda (name def)
               (when (eq (ir-def-kind def) :value)
                 (global-clone name)))
             defs)
    (let ((assigned (%make-lowered :defs *defs* :types (lowered-types lowered)
                                   :root (global-clone (lowered-root lowered))
                                   :stack-count *stack-count*)))
      (check-lowered assigned)
      (check-assigned assigned)
      assigned)))
