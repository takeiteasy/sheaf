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

;;; Storage

(defparameter *resizable-types* '(coalton/vector:vector coalton/hashtable:hashtable)
  "Types whose values grow after construction.")

;; TODO: these size classes stand in for the type syntax and the meaning of Cell,
;; Vector and Hashtable. A recursive data type is a fixed handle whose cells live
;; in a pool, and nothing is variable-size.
;; https://todo.sr.ht/~takeiteasy/sheaf/24
(defun size-class (type)
  "TYPE's size class, `:fixed` or `:resizable`."
  (if (member (ir-type-name type) *resizable-types*) :resizable :fixed))

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
                  (pooled-p type)
                  (and (some #'holds-storage-p
                             (ecase (ir-type-kind type)
                               (:data (field-types type))
                               (:reference (ir-type-args type))
                               (:scalar '())))
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

(defun locals-of (atoms)
  (loop :for atom :in atoms
        :when (typep atom 'ir-local) :collect (ir-local-name atom)))

(defun flowing-locals (node)
  "The locals whose contents the value of NODE may hold."
  (etypecase node
    (ir-local (list (ir-local-name node)))
    ((or ir-literal ir-global) '())
    (ir-let (flowing-locals (ir-let-body node)))
    (ir-construct (locals-of (ir-construct-args node)))
    (ir-call (locals-of (ir-call-args node)))
    (ir-prim (locals-of (ir-prim-args node)))
    (ir-foreign (mapcar #'cdr (ir-foreign-vars node)))
    (ir-match (mapcan (lambda (branch) (flowing-locals (ir-branch-body branch)))
                      (ir-match-branches node)))))

;; TODO: a value reaches the result when any storage-holding value flows into
;; it, so a cell may be built in the result's destination that roots would let
;; a local pool hold. Stores through a Cell are not followed.
;; https://todo.sr.ht/~takeiteasy/sheaf/25
(defun reaching-names (def)
  "A set of the names in DEF whose values may reach its result."
  (let ((flows (make-hash-table :test 'eq)))
    (labels ((flow (to from)
               (when from
                 (setf (gethash to flows) (append from (gethash to flows)))))
             (collect (node)
               (etypecase node
                 ((or ir-atom ir-construct ir-call ir-prim ir-foreign))
                 (ir-let
                  (when (holds-storage-p (ir-node-type (ir-let-value node)))
                    (flow (ir-let-name node) (flowing-locals (ir-let-value node))))
                  (collect (ir-let-value node))
                  (collect (ir-let-body node)))
                 (ir-match
                  (let ((scrutinee (ir-match-scrutinee node)))
                    (dolist (branch (ir-match-branches node))
                      (when (typep scrutinee 'ir-local)
                        (loop :for (name . type) :in (pattern-bindings (ir-branch-pattern branch))
                              :when (holds-storage-p type)
                                :do (flow name (list (ir-local-name scrutinee)))))
                      (collect (ir-branch-body branch))))))))
      (collect (ir-def-body def))
      (when (holds-storage-p (ir-def-result def))
        (flow :result (flowing-locals (ir-def-body def)))))
    (let ((reached (make-hash-table :test 'eq))
          (pending (list :result)))
      (loop :while pending
            :do (dolist (from (gethash (pop pending) flows))
                  (unless (gethash from reached)
                    (setf (gethash from reached) t)
                    (push from pending))))
      reached)))

(defun local-site-p (def reached)
  "Whether DEF builds storage that does not reach its result, which a pool of
its own holds."
  (labels ((scan (node reaches)
             (etypecase node
               ((or ir-atom ir-prim ir-foreign) nil)
               (ir-let (or (and (not (resizable-node-p (ir-let-value node)))
                                (scan (ir-let-value node) (gethash (ir-let-name node) reached)))
                           (scan (ir-let-body node) reaches)))
               (ir-construct (and (not reaches) (pooled-p (ir-node-type node))))
               (ir-call (and (not reaches) (holds-storage-p (ir-node-type node))))
               (ir-match (some (lambda (branch) (scan (ir-branch-body branch) reaches))
                               (ir-match-branches node))))))
    (scan (ir-def-body def) (holds-storage-p (ir-def-result def)))))

(defun builds-storage-p (def)
  "Whether DEF builds any storage at all."
  (block found
    (walk-ir (lambda (node)
               (when (typecase node
                       (ir-construct (pooled-p (ir-node-type node)))
                       (ir-call (holds-storage-p (ir-node-type node)))
                       (ir-let (resizable-node-p (ir-let-value node))))
                 (return-from found t)))
             (ir-def-body def))
    nil))

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

(defun clone-def (name destination region busy)
  "The name of the clone of the def NAME for a context: the stack its result is
built on, the region its cycle allocates in, and the stacks in use on entry."
  (let ((key (list name destination region busy)))
    (or (gethash key *clones*)
        (let ((clone (make-symbol (format nil "~a[~@[dest ~d ~]~@[region ~d ~]busy~{ ~d~}]"
                                          (symbol-name name) destination region busy))))
          (setf (gethash key *clones*) clone)
          (setf (gethash clone *defs*)
                (assign-def (gethash name (lowered-defs *lowered*)) clone destination region busy))
          clone))))

(defun global-clone (name)
  (clone-def name (gethash name *globals*) nil *global-busy*))

(defun assign-def (def clone destination region entry-busy)
  (let* ((cycle (gethash (ir-def-name def) *cycles*))
         (reached (reaching-names def))
         (pool (if cycle
                   region
                   (and (local-site-p def reached) (take-index entry-busy))))
         (new (copy-ir-def def)))
    (labels ((storage-stack (reaches)
               (or (if reaches destination pool)
                   (error "~s has no stack to build ~:[a local~;its result~] in"
                          (ir-def-name def) reaches)))
             (rewrite-atom (node)
               (if (typep node 'ir-global)
                   (let ((copy (copy-ir-global node)))
                     (setf (ir-global-name copy) (global-clone (ir-global-name node)))
                     copy)
                   node))
             (rewrite-call (node reaches busy owner)
               (let* ((callee (ir-call-function node))
                      (callee-cycle (gethash callee *cycles*))
                      (dest (and (holds-storage-p (ir-node-type node))
                                 (or owner (storage-stack reaches))))
                      (copy (copy-ir-call node)))
                 (setf (ir-call-args copy) (mapcar #'rewrite-atom (ir-call-args node))
                       (ir-call-destination copy) dest
                       (ir-call-function copy)
                       (cond ((and callee-cycle (eq callee-cycle cycle))
                              (when (set-difference busy entry-busy)
                                (error "~s keeps a growable local across a call back into its recursive cycle: https://todo.sr.ht/~~takeiteasy/sheaf/10"
                                       (ir-def-name def)))
                              (clone-def callee dest region entry-busy))
                             (callee-cycle
                              (let* ((owned (and (null dest) (cdr callee-cycle) (take-index busy)))
                                     (callee-region (or dest owned)))
                                (setf (ir-call-region copy) owned)
                                (clone-def callee dest callee-region (with-index busy callee-region))))
                             (t (clone-def callee dest nil busy))))
                 copy))
             (rewrite (node reaches busy owner)
               (etypecase node
                 ((or ir-literal ir-local ir-foreign) node)
                 (ir-global (rewrite-atom node))
                 (ir-let
                  (let* ((value (ir-let-value node))
                         (reaches-value (gethash (ir-let-name node) reached))
                         (stack (and (resizable-node-p value) (not reaches-value) (take-index busy)))
                         (inner (with-index busy stack))
                         (copy (copy-ir-let node)))
                    (setf (ir-let-stack copy) stack
                          (ir-let-value copy) (rewrite value reaches-value inner stack)
                          (ir-let-body copy) (rewrite (ir-let-body node) reaches inner nil))
                    copy))
                 (ir-call (rewrite-call node reaches busy owner))
                 (ir-construct
                  (let ((copy (copy-ir-construct node)))
                    (setf (ir-construct-args copy) (mapcar #'rewrite-atom (ir-construct-args node)))
                    (when (pooled-p (ir-node-type node))
                      (setf (ir-construct-stack copy) (storage-stack reaches)))
                    copy))
                 (ir-prim
                  (let ((copy (copy-ir-prim node)))
                    (setf (ir-prim-args copy) (mapcar #'rewrite-atom (ir-prim-args node)))
                    copy))
                 (ir-match
                  (let ((copy (copy-ir-match node)))
                    (setf (ir-match-scrutinee copy) (rewrite-atom (ir-match-scrutinee node))
                          (ir-match-branches copy)
                          (loop :for branch :in (ir-match-branches node)
                                :collect (let ((branch-copy (copy-ir-branch branch)))
                                           (setf (ir-branch-body branch-copy)
                                                 (rewrite (ir-branch-body branch) reaches busy nil))
                                           branch-copy)))
                    copy)))))
      (setf (ir-def-name new) clone
            (ir-def-source new) (ir-def-name def)
            (ir-def-destination new) destination
            (ir-def-pool new) (and (not cycle) pool)
            (ir-def-body new) (rewrite (ir-def-body def)
                                       (holds-storage-p (ir-def-result def))
                                       (with-index entry-busy pool)
                                       nil))
      new)))

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
                      (ir-call (when (holds-storage-p (ir-node-type node))
                                 (check-index (ir-call-destination node) "a call's result"))
                               (when (ir-call-region node)
                                 (check-index (ir-call-region node) "a call's region")))
                      (ir-let (when (ir-let-stack node)
                                (check-index (ir-let-stack node) "a growable local")))))
                  (ir-def-body def))
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
         (*cycles* (find-cycles defs))
         (*globals* (make-hash-table :test 'eq))
         (*global-busy* '())
         (*stack-count* 0)
         (root (gethash (lowered-root lowered) defs)))
    (flet ((take-global (name)
             (let ((index (take-index *global-busy*)))
               (setf *global-busy* (with-index *global-busy* index)
                     (gethash name *globals*) index))))
      (maphash (lambda (name def)
                 (when (and (eq (ir-def-kind def) :value) (holds-storage-p (ir-def-result def)))
                   (take-global name)))
               defs)
      (when (and (eq (ir-def-kind root) :function) (holds-storage-p (ir-def-result root)))
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
