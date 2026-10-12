;;;; roots.lisp
;;;; Root and lifetime checking after Goose's spec section 9.2: every reference
;;;; has a static root, and a store of a reference is rejected unless its root
;;;; outlives the storage it is stored into, which stack assignment decides.

(in-package #:sheaf)

(defvar *reference-types*)
(defvar *here* nil "The location of the node being checked, for errors.")

(defstruct (root-summary (:constructor make-root-summary (&key result stores locals reached bare-calls slice-free-calls call-roots)))
  "What a def does with the roots of its arguments. RESULT lists what its result
may point into: a parameter index, `:global`, or `:fresh` for storage it builds
in its caller's destinations. STORES lists the stores it leaves its caller to
check, each a parameter index, the parameter index or `:global` it is stored
into, and the chain of places that justifies it. LOCALS maps each name to its
roots. REACHED lists the locals whose storage the result holds, which are built
in the caller's destinations. BARE-CALLS lists the calls whose arguments point
only into parameters and globals, and SLICE-FREE-CALLS those whose arguments
reach no growable value the def built. CALL-ROOTS lists each call with the roots
of its arguments and everything those reach."
  (result '()) (stores '()) (locals nil) (reached '()) (bare-calls '()) (slice-free-calls '())
  (call-roots '()))

;; op, how it treats roots, and the arguments it uses
(defparameter *prim-roots*
  '((:fixed-new :allocates 0) (:array-new :allocates 1) (:grow-new :allocates) (:stack-new :allocates)
    (:cell-new :allocates 0) (:vector-new :allocates)
    (:hashtable-new :allocates) (:hashtable-with-capacity :allocates)
    (:vector-push :stores 0 1) (:vector-set :stores 1 2) (:hashtable-set :stores 2 0)
    (:grow-push :stores 1 0) (:stack-push :stores 1 0) (:ref-set :stores 1 0) (:elements-set :stores 2 0)
    (:cell-write :stores 1 0 :returns-stored) (:cell-swap :stores 1 0 :returns-container)
    (:stack-pop :reads 0) (:ref-get :reads 0) (:elements-index :reads 0) (:cell-read :reads 0)
    (:vector-pop :reads 0) (:vector-index :reads 1) (:vector-index-unsafe :reads 1) (:hashtable-get :reads 0)
    (:elements-ref :reads 0) (:elements-slice :reads 0))
  "How the prelude's primitives treat roots. An allocating primitive makes a root
and may store its initial element, a storing one stores the argument at its
first index into the one at its second and gives back nothing, the value stored
or the roots of its container, and a reading one gives back the roots of its
container.")

(defun holds-reference-p (type)
  "Whether a value of TYPE is, or holds, an array, slice or reference."
  (multiple-value-bind (known found) (gethash type *reference-types*)
    (if found
        known
        (progn
          (setf (gethash type *reference-types*) nil)
          (setf (gethash type *reference-types*)
                (ecase (ir-type-kind type)
                  ((:view :storage) t)
                  (:scalar nil)
                  (:data (and (some #'holds-reference-p (field-types type)) t))))))))

(defun tail-node (node)
  "The expression a body of nested lets ends in."
  (if (typep node 'ir-let) (tail-node (ir-let-body node)) node))

(defun let-lifetimes (node reached)
  "A table from the name of each let in NODE to when its storage is freed, as
stack assignment gives it out: -1 for the caller's stacks, 0 for the function's
pool, or the number of `match` branches around a growable local freed when its
branch ends. REACHED holds the locals the result holds."
  (let ((lifetimes (make-hash-table :test 'eq)))
    (labels ((collect (node depth owner)
               "OWNER is when the stacks a returned let takes are freed."
               (etypecase node
                 ((or ir-atom ir-prim ir-call ir-construct ir-foreign) nil)
                 (ir-let
                  (let* ((value (ir-let-value node))
                         (returned (and (not (gethash (ir-let-name node) reached)) (returns-own-name-p node)))
                         (lifetime (cond ((gethash (ir-let-name node) reached) -1)
                                         (returned owner)
                                         ((or (plusp (tail-need value)) (tail-banked-p value)) depth)
                                         (t 0))))
                    (setf (gethash (ir-let-name node) lifetimes) lifetime)
                    (collect value depth lifetime)
                    (collect (ir-let-body node) depth owner)))
                 (ir-match
                  (dolist (branch (ir-match-branches node))
                    (collect (ir-branch-body branch) (1+ depth) owner)))
                 ((or ir-throw ir-return ir-break ir-continue) nil)
                 (ir-block (collect (ir-block-body node) depth owner))
                 (ir-loop
                  (when (ir-loop-test node) (collect (ir-loop-test node) (1+ depth) owner))
                  (dolist (step (ir-loop-steps node))
                    (when step (collect step (1+ depth) owner)))
                  (collect (ir-loop-body node) (1+ depth) owner)
                  (collect (ir-loop-returns node) depth owner))
                 (ir-catch
                  (dolist (body (cons (ir-catch-body node) (mapcar #'ir-catch-branch-body (ir-catch-branches node))))
                    (collect body (1+ depth) owner)))
                 (ir-protect
                  (collect (ir-protect-body node) (1+ depth) owner)
                  (collect (ir-protect-cleanup node) (1+ depth) owner)))))
      (collect node 0 -1))
    lifetimes))

(defun root-text (root def-name)
  (cond ((integerp root) (format nil "parameter ~d of ~a" root def-name))
        ((eq root :global) "a global")
        ((eq root :fresh) "the result")
        (t (format nil "~a" root))))

(defun summary-equal (a b)
  (and (null (set-exclusive-or (root-summary-result a) (root-summary-result b) :test #'eql))
       (null (set-exclusive-or (mapcar (lambda (s) (list (first s) (second s))) (root-summary-stores a))
                               (mapcar (lambda (s) (list (first s) (second s))) (root-summary-stores b))
                               :test #'equal))))

(defun summarize (def summaries report)
  "The `root-summary` of DEF given the summaries of the defs it calls. When REPORT
is true a store that no caller could make safe is an error."
  (let* ((def-name (ir-def-name def))
         (depths (make-hash-table :test 'eq))
         (contents (make-hash-table :test 'eq))
         (origins (make-hash-table :test 'eq))
         (links (make-hash-table :test 'eq))
         (locals (make-hash-table :test 'eq))
         (stores '())
         (bare-calls '())
         (call-roots '())
         (growable-lets (make-hash-table :test 'eq))
         (pending '())
         (branches '())
         (returned (make-hash-table :test 'eq))
         (*here* (ir-def-location def)))
    (labels ((entry (text)
               (list def-name text *here*))
             (fail (chain control &rest args)
               (when report
                 (error 'specialization-error :message (apply #'format nil control args) :chain chain)))
             (reference-entry (atom)
               (let ((origin (and (typep atom 'ir-local) (gethash (ir-local-name atom) origins))))
                 (if origin
                     (list def-name (first origin) (second origin))
                     (entry (type-text (ir-node-type atom))))))
             (merge-roots (&rest sets)
               (remove-duplicates (apply #'append sets) :test #'eql))
             (add-contents (container roots)
               (when (and roots (symbolp container) (not (eq container :global)))
                 (setf (gethash container contents) (merge-roots roots (gethash container contents)))))
             (add-links (container roots)
               (when roots
                 (setf (gethash container links) (merge-roots roots (gethash container links)))))
             (defer (from into trail)
               (unless (find-if (lambda (store) (and (eql from (first store)) (eql into (second store))))
                                stores)
                 (push (list from into trail) stores)))
             (check-store (values containers value-atom trail)
               (dolist (container containers)
                 (dolist (value values)
                   (cond ((eq value :global))
                         ((integerp value)
                          (cond ((integerp container) (unless (= value container) (defer value container trail)))
                                ((eq container :global) (defer value :global trail))))
                         ((eq container :fresh))
                         ((or (integerp container) (eq container :global))
                          (fail (cons (reference-entry value-atom) trail)
                                "Root checking rejects storing a reference rooted at ~a into storage rooted at ~a, which outlives it"
                                (root-text value def-name) (root-text container def-name)))
                         (report
                          (push (list value container (cons (reference-entry value-atom) trail)) pending)))
                   (add-contents container (list value)))))
             (walk (node env depth owner)
               (let ((*here* (or (ir-node-location node) *here*)))
                 (etypecase node
                   (ir-literal '())
                   (ir-global (and (holds-storage-p (ir-node-type node)) (list :global)))
                   (ir-local (cdr (assoc (ir-local-name node) env)))
                   (ir-let (walk-let node env depth owner))
                   (ir-construct (walk-construct node env depth owner))
                   (ir-prim (walk-prim node env depth owner))
                   (ir-call (walk-call node env depth owner))
                   (ir-foreign (walk-foreign node env owner))
                   (ir-match (walk-match node env depth owner))
                   (ir-throw '())
                   (ir-block (walk-block node env depth owner))
                   (ir-loop (walk-loop node env depth owner))
                   ((or ir-break ir-continue) '())
                   (ir-return (walk-return node env depth owner))
                   (ir-catch (walk-catch node env depth owner))
                   (ir-protect (walk-protect node env depth owner)))))
             (walk-all (atoms env depth owner)
               (apply #'merge-roots (mapcar (lambda (atom) (walk atom env depth owner)) atoms)))
             (walk-foreign (node env owner)
               (let ((held (merge-roots
                            (loop :for (nil . var) :in (ir-foreign-vars node)
                                  :append (cdr (assoc var env))))))
                 (if (resizable-node-p node)
                     (let ((root (fresh-root owner)))
                       (add-links root held)
                       (list root))
                     held)))
             (walk-construct (node env depth owner)
               (let ((held (walk-all (ir-construct-args node) env depth owner)))
                 (if (pooled-p (ir-node-type node))
                     (let ((root (fresh-root owner)))
                       (add-links root held)
                       (list root))
                     held)))
             (walk-let (node env depth owner)
               (let ((name (ir-let-name node))
                     (type (ir-node-type (ir-let-value node))))
                 (setf (gethash name growable-lets) (holds-growable-p type)
                       (gethash name depths) depth
                       (gethash name origins) (list (format nil "a value of type ~a" (type-text (ir-node-type (ir-let-value node))))
                                                    (or (ir-node-location (ir-let-value node)) *here*)))
                 (let ((roots (let ((reached (walk (ir-let-value node) env depth name)))
                                (and (holds-storage-p type) reached))))
                   (setf (gethash name locals) roots)
                   (walk (ir-let-body node) (acons name roots env) depth owner))))
             (fresh-root (owner)
               (if (eq owner :fresh) :fresh owner))
             (walk-prim (node env depth owner)
               (destructuring-bind (&optional kind first-index second-index returns)
                   (cdr (assoc (ir-prim-op node) *prim-roots*))
                 (let ((args (ir-prim-args node)))
                   (ecase kind
                     ((nil) '())
                     (:allocates
                      (let ((root (fresh-root owner)))
                        (when first-index
                          (add-contents root (walk (nth first-index args) env depth owner)))
                        (list root)))
                     (:stores
                      (let ((value (nth first-index args)))
                        (when (holds-storage-p (ir-node-type value))
                          (check-store (walk value env depth owner)
                                       (walk (nth second-index args) env depth owner)
                                       value
                                       (list (entry (format nil "the store ~(~a~)" (ir-prim-op node)))))))
                      (and (holds-reference-p (ir-node-type node))
                           (case returns
                             (:returns-stored (walk (nth first-index args) env depth owner))
                             (:returns-container (walk (nth second-index args) env depth owner)))))
                     (:reads
                      (and (holds-storage-p (ir-node-type node))
                           (walk (nth first-index args) env depth owner)))))))
             (walk-call (node env depth owner)
               (let* ((summary (or (gethash (ir-call-function node) summaries) (make-root-summary)))
                      (args (ir-call-args node))
                      (arg-roots (mapcar (lambda (arg) (walk arg env depth owner)) args))
                      (call-entry (entry (format nil "the call of ~a" (ir-call-function node)))))
                 (when (every (lambda (roots) (every (lambda (root) (or (integerp root) (eq root :global))) roots))
                              arg-roots)
                   (push node bare-calls))
                 (push (cons node arg-roots) call-roots)
                 (loop :for (from into trail) :in (root-summary-stores summary)
                       :do (check-store (nth from arg-roots)
                                        (if (eq into :global) (list :global) (nth into arg-roots))
                                        (nth from args)
                                        (cons call-entry trail)))
                 (when (holds-storage-p (ir-node-type node))
                   (let* ((reached (loop :for root :in (root-summary-result summary)
                                         :append (cond ((integerp root) (nth root arg-roots))
                                                       ((eq root :global) (list :global)))))
                          (fresh (and (member :fresh (root-summary-result summary)) (fresh-root owner))))
                     (when fresh (add-contents fresh reached))
                     (merge-roots reached (and fresh (list fresh)))))))
             (walk-match (node env depth owner)
               (let* ((scrutinee (walk (ir-match-scrutinee node) env depth owner))
                      (match-entry (entry "the match that receives it"))
                      (result '()))
                 (dolist (branch (ir-match-branches node))
                   (let ((roots (walk (ir-branch-body branch) (branch-env branch scrutinee env) (1+ depth) owner)))
                     (when report
                       (let ((tail (tail-node (ir-branch-body branch))))
                         (push (list roots depth
                                     (list (list def-name (format nil "the reference of type ~a leaving the branch"
                                                                  (type-text (ir-node-type tail)))
                                                 (or (ir-node-location tail) *here*))
                                           match-entry))
                               branches)))
                     (setf result (merge-roots roots result))))
                 result))
             (walk-scope (body what-entry env depth owner)
               "The roots BODY, a branch of a catch or protect, gives back. When
REPORT is true, notes that they must not outlive what BODY frees."
               (let ((roots (walk body env (1+ depth) owner)))
                 (when report
                   (let ((tail (tail-node body)))
                     (push (list roots depth
                                 (list (list def-name (format nil "the reference of type ~a leaving the branch"
                                                              (type-text (ir-node-type tail)))
                                             (or (ir-node-location tail) *here*))
                                       what-entry))
                           branches)))
                 roots))
             (walk-block (node env depth owner)
               (let ((label (ir-block-label node)))
                 (setf (gethash label returned) (list '() depth))
                 (merge-roots (walk (ir-block-body node) env depth owner)
                              (first (gethash label returned)))))
             (walk-loop (node env depth owner)
               (let* ((vars (ir-loop-vars node))
                      (inits (mapcar (lambda (init) (walk init env depth owner)) (ir-loop-inits node)))
                      (roots inits)
                      (inner env))
                 (loop :for (name . type) :in vars
                       :do (setf (gethash name depths) depth
                                 (gethash name growable-lets) (holds-growable-p type)
                                 (gethash name origins) (list (format nil "the loop variable of type ~a" (type-text type))
                                                              *here*)))
                 (loop :for changed := nil
                       :do (setf inner env)
                           (loop :for (name . nil) :in vars
                                 :for var-roots :in roots
                                 :do (setf inner (acons name var-roots inner)))
                           (let ((next (loop :for (name . type) :in vars
                                             :for step :in (ir-loop-steps node)
                                             :for init :in inits
                                             :collect (merge-roots
                                                       init
                                                       (and step (holds-storage-p type)
                                                            (let ((stepped (walk step inner (1+ depth) name)))
                                                              (when report
                                                                (push (list stepped depth
                                                                            (list (list def-name (format nil "the loop variable of type ~a" (type-text type))
                                                                                        (or (ir-node-location step) *here*))
                                                                                  (entry "the loop that carries it")))
                                                                      branches))
                                                              stepped))))))
                             (setf changed (notevery (lambda (a b) (null (set-exclusive-or a b))) next roots)
                                   roots next))
                       :while changed)
                 (loop :for (name . nil) :in vars
                       :for var-roots :in roots
                       :do (setf (gethash name locals) var-roots))
                 (when (ir-loop-test node) (walk (ir-loop-test node) inner (1+ depth) owner))
                 (walk (ir-loop-body node) inner (1+ depth) owner)
                 (walk (ir-loop-returns node) inner depth owner)))
             (walk-return (node env depth owner)
               (let ((roots (walk (ir-return-value node) env depth owner))
                     (target (gethash (ir-return-label node) returned)))
                 (setf (first target) (merge-roots roots (first target)))
                 (when report
                   (push (list roots (second target)
                               (list (list def-name (format nil "the reference of type ~a returned"
                                                            (type-text (ir-node-type (ir-return-value node))))
                                           *here*)
                                     (entry "the block that receives it")))
                         branches))
                 '()))
             (walk-catch (node env depth owner)
               (let ((catch-entry (entry "the catch that receives it"))
                     (result '()))
                 (dolist (body (cons (ir-catch-body node) (mapcar #'ir-catch-branch-body (ir-catch-branches node)))
                               result)
                   (setf result (merge-roots (walk-scope body catch-entry env depth owner) result)))))
             (walk-protect (node env depth owner)
               (let ((roots (walk-scope (ir-protect-body node) (entry "the protect that receives it") env depth owner)))
                 (walk (ir-protect-cleanup node) env (1+ depth) owner)
                 roots))
             (branch-env (branch scrutinee env)
               (loop :for (name . type) :in (pattern-bindings (ir-branch-pattern branch))
                     :when (holds-storage-p type)
                       :do (setf (gethash name origins)
                                 (list (format nil "a value of type ~a bound by a match" (type-text type)) *here*)
                             env (acons name scrutinee env)))
               env)
             (closure (roots tables)
               "ROOTS and everything the storage among them holds, following TABLES."
               (let ((seen '()) (pending (copy-list roots)))
                 (loop :while pending
                       :do (let ((root (pop pending)))
                             (unless (member root seen)
                               (push root seen)
                               (dolist (table tables)
                                 (dolist (held (gethash root table))
                                   (push held pending))))))
                 seen)))
      (dolist (param (ir-def-params def))
        (setf (gethash (car param) origins) (list "the parameter" (ir-def-location def))))
      (let* ((env (loop :for (nil . type) :in (ir-def-params def)
                        :for index :from 0
                        :when (holds-storage-p type)
                          :collect (cons (car (nth index (ir-def-params def))) (list index))))
             (roots (let ((reached (walk (ir-def-body def) env 0 :fresh)))
                      (and (holds-storage-p (ir-def-result def)) reached)))
             (reached (remove-if (lambda (root) (or (not (symbolp root)) (keywordp root)))
                                 (closure roots (list contents links))))
             (result '()))
        (when report
          (let ((live (make-hash-table :test 'eq)))
            (dolist (name reached)
              (setf (gethash name live) t))
            (let ((lifetimes (let-lifetimes (ir-def-body def) live)))
              (flet ((freed-at (roots)
                       (reduce #'max (closure roots (list links contents))
                               :key (lambda (root) (gethash root lifetimes -1))
                               :initial-value -1)))
                (loop :for (value container chain) :in (nreverse pending)
                      :when (> (freed-at (list value)) (gethash container depths))
                        :do (fail chain "Root checking rejects storing a reference rooted at ~a into storage rooted at ~a, which outlives it"
                                  (root-text value def-name) (root-text container def-name)))
                (loop :for (branch-roots branch-depth chain) :in (nreverse branches)
                      :do (dolist (root (closure branch-roots (list links contents)))
                            (when (> (gethash root lifetimes -1) branch-depth)
                              (fail chain "Root checking rejects a reference rooted at ~a that outlives the branch it is made in"
                                    (root-text root def-name)))))))))
        (dolist (root (closure roots (list links)))
          (cond ((integerp root) (pushnew root result))
                ((eq root :global) (pushnew :global result))
                (t (pushnew :fresh result)
                   (dolist (held (remove root (closure (list root) (list contents))))
                     (cond ((integerp held) (pushnew held result))
                           ((eq held :global) (pushnew :global result)))))))
        (let ((call-reach (loop :for (call . arg-roots) :in call-roots
                                :collect (list call arg-roots
                                               (closure (apply #'append arg-roots) (list contents links))))))
          (make-root-summary :result (nreverse result)
                             :stores (reverse stores)
                             :locals locals
                             :reached reached
                             :bare-calls bare-calls
                             :call-roots call-reach
                             :slice-free-calls
                             (loop :for (call nil reach) :in call-reach
                                   :when (every (lambda (root)
                                                  (or (integerp root) (eq root :global)
                                                      (not (gethash root growable-lets t))))
                                                reach)
                                     :collect call)))))))

(defun root-summaries (lowered &key report)
  "A table from the name of each def in LOWERED to its `root-summary`. When
REPORT is true a store that no caller could make safe, a store of a reference
into storage that outlives what it points into, or a reference that leaves a
branch which frees what it points into, is an error."
  (let* ((*lowered* lowered)
         (*storage* (make-hash-table :test 'eq))
         (*reference-types* (make-hash-table :test 'eq))
         (defs (lowered-defs lowered))
         (summaries (make-hash-table :test 'eq))
         (sccs (strongly-connected-components
                (loop :for name :being :the :hash-keys :of defs :collect name)
                (lambda (name) (callees (gethash name defs))))))
    (dolist (scc sccs)
      (dolist (name scc)
        (setf (gethash name summaries) (make-root-summary)))
      (loop :for changed := nil
            :do (dolist (name scc)
                  (let ((new (summarize (gethash name defs) summaries nil)))
                    (unless (summary-equal new (gethash name summaries))
                      (setf changed t))
                    (setf (gethash name summaries) new)))
            :while changed))
    (when report
      (let* ((*summaries* summaries)
             (*placements* (compute-placements defs)))
        (dolist (scc sccs)
          (dolist (name scc)
            (setf (gethash name summaries) (summarize (gethash name defs) summaries t))))))
    summaries))

(defun check-roots (lowered)
  "Signal a `specialization-error` naming both the reference and the store unless
every store of a reference in LOWERED goes into storage that is not freed after
what the reference points into, and no reference leaves a branch whose growable
stacks are freed when it ends.
Returns a table from the name of each def to its `root-summary`."
  (root-summaries lowered :report t))
