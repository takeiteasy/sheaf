;;;; roots.lisp
;;;; Root and lifetime checking after Goose's spec section 9.2: every reference
;;;; has a static root, and a store of a reference is rejected unless its root
;;;; outlives the storage it is stored into.

(in-package #:sheaf)

(defvar *reference-types*)
(defvar *here* nil "The location of the node being checked, for errors.")

(defstruct (root-summary (:constructor make-root-summary (&key result stores locals reached)))
  "What a def does with the roots of its arguments. RESULT lists what its result
may point into: a parameter index, `:global`, or `:fresh` for storage it builds
in its caller's destination. STORES lists the stores it leaves its caller to
check, each a parameter index, the parameter index or `:global` it is stored
into, and the chain of places that justifies it. LOCALS maps each name to its
roots. REACHED lists the locals whose storage the result holds, which are built
in the caller's destination."
  (result '()) (stores '()) (locals nil) (reached '()))

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
         (reference-roots (make-hash-table :test 'eq))
         (sites (make-hash-table :test 'eq))
         (locals (make-hash-table :test 'eq))
         (stores '())
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
                         ((or (integerp container) (eq container :global)
                              (> (gethash value depths) (gethash container depths)))
                          (fail (cons (reference-entry value-atom) trail)
                                "Root checking rejects storing a reference rooted at ~a into storage rooted at ~a, which outlives it"
                                (root-text value def-name) (root-text container def-name))))
                   (add-contents container (list value))
                   (unless (gethash value sites)
                     (setf (gethash value sites) (cons (reference-entry value-atom) trail))))))
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
                   (ir-match (walk-match node env depth owner)))))
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
                 (setf (gethash name depths) depth
                       (gethash name reference-roots) (and (holds-reference-p type) (not (pooled-p type)))
                       (gethash name origins) (list (format nil "a value of type ~a" (type-text (ir-node-type (ir-let-value node))))
                                                    (or (ir-node-location (ir-let-value node)) *here*)))
                 (let ((roots (let ((reached (walk (ir-let-value node) env (1+ depth) name)))
                                (and (holds-storage-p type) reached))))
                   (setf (gethash name locals) roots)
                   (walk (ir-let-body node) (acons name roots env) (1+ depth) owner))))
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
                          (let* ((initial (nth first-index args))
                                 (reached (walk initial env depth owner)))
                            (add-contents root reached)
                            (dolist (held reached)
                              (unless (gethash held sites)
                                (setf (gethash held sites)
                                      (list (reference-entry initial)
                                            (entry "the array it initialises")))))))
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
                   (let ((roots (walk (ir-branch-body branch) (branch-env branch scrutinee env) depth owner)))
                     (unless (eq owner :fresh)
                       (dolist (root (closure roots (list links)))
                         (when (and (symbolp root) (gethash root reference-roots)
                                    (not (member root (list :global :fresh owner)))
                                    (>= (gethash root depths) depth))
                           (fail (list (let ((tail (tail-node (ir-branch-body branch))))
                                         (list def-name (format nil "the reference of type ~a leaving the branch"
                                                                (type-text (ir-node-type tail)))
                                               (or (ir-node-location tail) *here*)))
                                       match-entry)
                                 "Root checking rejects a reference rooted at ~a that outlives the branch it is made in"
                                 (root-text root def-name)))))
                     (setf result (merge-roots roots result))))
                 result))
             (branch-env (branch scrutinee env)
               (loop :for (name . type) :in (pattern-bindings (ir-branch-pattern branch))
                     :when (holds-storage-p type)
                       :do (setf (gethash name origins)
                                 (list (format nil "a value of type ~a bound by a match" (type-text type)) *here*)
                             env (acons name scrutinee env)))
               env)
             (root-entry (root)
               (let ((origin (gethash root origins)))
                 (if origin
                     (list def-name (first origin) (second origin))
                     (entry "the root"))))
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
             (result '()))
        ;; Stack assignment builds every local in REACHED in the caller's
        ;; destination, so the rules below are stricter than it needs.
        ;; TODO: reject only what stack assignment does not place in the
        ;; destination.
        ;; https://todo.sr.ht/~takeiteasy/sheaf/28
        (dolist (root (closure roots (list links)))
          (cond ((integerp root) (pushnew root result))
                ((eq root :global) (pushnew :global result))
                (t (pushnew :fresh result)
                   (dolist (held (remove root (closure (list root) (list contents))))
                     (cond ((integerp held) (pushnew held result))
                           ((eq held :global) (pushnew :global result))
                           ((eq held :fresh))
                           ((not (gethash held reference-roots)))
                           (t (fail (append (or (gethash held sites) (list (root-entry held)))
                                            (list (list def-name "the result" (ir-def-location def))))
                                    "Root checking rejects returning storage that holds a reference rooted at ~a"
                                    (root-text held def-name))))))))
        (make-root-summary :result (nreverse result)
                           :stores (reverse stores)
                           :locals locals
                           :reached (remove-if (lambda (root) (or (not (symbolp root)) (keywordp root)))
                                               (closure roots (list contents links))))))))

(defun root-summaries (lowered &key report)
  "A table from the name of each def in LOWERED to its `root-summary`. When
REPORT is true a store that no caller could make safe is an error."
  (let* ((*lowered* lowered)
         (*storage* (make-hash-table :test 'eq))
         (*reference-types* (make-hash-table :test 'eq))
         (defs (lowered-defs lowered))
         (summaries (make-hash-table :test 'eq)))
    (dolist (scc (strongly-connected-components
                  (loop :for name :being :the :hash-keys :of defs :collect name)
                  (lambda (name) (callees (gethash name defs)))))
      (dolist (name scc)
        (setf (gethash name summaries) (make-root-summary)))
      (loop :for changed := nil
            :do (dolist (name scc)
                  (let ((new (summarize (gethash name defs) summaries nil)))
                    (unless (summary-equal new (gethash name summaries))
                      (setf changed t))
                    (setf (gethash name summaries) new)))
            :while changed)
      (when report
        (dolist (name scc)
          (setf (gethash name summaries) (summarize (gethash name defs) summaries t)))))
    summaries))

(defun check-roots (lowered)
  "Signal a `specialization-error` naming both the reference and the store unless
every store of a reference in LOWERED goes into storage its root outlives.
Returns a table from the name of each def to its `root-summary`."
  (root-summaries lowered :report t))
