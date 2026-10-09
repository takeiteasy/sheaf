;;;; interpret.lisp
;;;; A reference interpreter for the IR, which defines what a lowered program means.

(in-package #:sheaf)

(defstruct (datum (:constructor make-datum (constructor fields &optional stack)))
  "A value of a data type. STACK is the data stack holding it, if it is a cell."
  constructor fields stack (freed nil))

(defstruct (storage (:include datum) (:constructor make-storage (constructor fields stack)))
  "An array. CONSTRUCTOR is how it is sized and FIELDS its elements, which are
shared by every value that holds the array.")

(defstruct (ref-value (:constructor make-ref-value (target index)))
  "A reference to one element of the array TARGET."
  target index)

(defstruct (slice-value (:constructor make-slice-value (target start end)))
  "A view of the elements of the array TARGET from START up to END."
  target start end)

(defstruct (data-stack (:conc-name stack-))
  "A simulated data stack: whether an owner holds it, the items it held when
that owner took it, and the cells pushed since."
  (owned nil) (mark '()) (items '()))

(defun named-constructor (lowered type name)
  "The constructor of TYPE whose symbol name is NAME."
  (find name (lowered-constructors lowered type)
        :key (lambda (constructor) (symbol-name (ir-constructor-name constructor)))
        :test #'string=))

(defun constructor-named (lowered type name)
  (ir-constructor-name (named-constructor lowered type name)))

(defun apply-primitive (lowered op args type)
  (destructuring-bind (&optional a b) args
    (flet ((bool (value)
             (make-datum (constructor-named lowered type (if value "TRUE" "FALSE")) '())))
      (ecase op
        (:ufix-add (+ a b))
        (:ufix-sub (- a b))
        (:ufix-mul (* a b))
        (:ufix-from-int (mod a +ufix-modulus+))
        (:ufix-eq (bool (= a b)))
        (:ufix-compare
         (make-datum (constructor-named lowered type (cond ((< a b) "LT") ((> a b) "GT") (t "EQ")))
                     '()))
        (:ordering-eq (bool (eq (datum-constructor a) (datum-constructor b))))))))

(defun match-pattern (pattern value env)
  "Whether VALUE matches PATTERN, and ENV extended with the variables it binds."
  (etypecase pattern
    (ir-pwild (values t env))
    (ir-pvar (values t (acons (ir-pvar-name pattern) value env)))
    (ir-plit (values (equal (ir-plit-value pattern) value) env))
    (ir-pas (multiple-value-bind (matched env) (match-pattern (ir-pas-pattern pattern) value env)
              (values matched (and matched (acons (ir-pas-name pattern) value env)))))
    (ir-pcon
     (if (eq (ir-pcon-constructor pattern) (datum-constructor value))
         (loop :for field :in (ir-pcon-patterns pattern)
               :for field-value :in (datum-fields value)
               :do (multiple-value-bind (matched next) (match-pattern field field-value env)
                     (unless matched (return (values nil env)))
                     (setf env next))
               :finally (return (values t env)))
         (values nil env)))))

(defun run-lowered (lowered)
  "Evaluate the root of LOWERED, a function of no arguments or a value. Raw Lisp
cannot be evaluated: https://todo.sr.ht/~takeiteasy/sheaf/12

A program with a stack assignment also runs on simulated data stacks and signals
an error on a stack with two owners, a cell pushed to a stack without one, or a
cell read after its stack was freed. Returns the value, and the number of stacks
the run touched when the program has a stack assignment."
  (let* ((defs (lowered-defs lowered))
         (count (lowered-stack-count lowered))
         (stacks (and count (coerce (loop :repeat count :collect (make-data-stack)) 'vector)))
         (touched 0)
         (globals (make-hash-table :test 'eq)))
    (labels ((global (name)
               (or (gethash name defs) (error "~s is not defined" name)))
             (stack (index)
               (unless (< -1 index count)
                 (error "Stack ~d is not one of the ~d stacks" index count))
               (aref stacks index))
             (claim (index)
               (when stacks
                 (let ((stack (stack index)))
                   (when (stack-owned stack)
                     (error "Stack ~d has two owners" index))
                   (setf (stack-owned stack) t
                         (stack-mark stack) (stack-items stack)
                         touched (max touched (1+ index))))))
             (release (index)
               (when stacks
                 (let ((stack (stack index)))
                   (loop :until (eq (stack-items stack) (stack-mark stack))
                         :do (setf (datum-freed (pop (stack-items stack))) t))
                   (setf (stack-owned stack) nil))))
             (push-cell (index cell)
               (let ((stack (stack index)))
                 (unless (stack-owned stack)
                   (error "A cell was pushed to stack ~d, which has no owner" index))
                 (push cell (stack-items stack))))
             (live (value what)
               (let ((owner (typecase value
                              (datum value)
                              (ref-value (ref-value-target value))
                              (slice-value (slice-value-target value)))))
                 (when (and owner (datum-freed owner))
                   (error "~a a cell on stack ~d after it was freed" what (datum-stack owner))))
               value)
             (allocate (class elements node)
               (let* ((stack (ir-prim-stack node))
                      (array (make-storage class elements stack)))
                 (when stack (push-cell stack array))
                 array))
             (prelude (op args node)
               (let ((type (ir-node-type node)))
                 (labels ((unit ()
                            (make-datum (constructor-named lowered type "UNIT") '()))
                          (elements (n initial)
                            (make-array n :adjustable t :fill-pointer n :initial-element initial))
                          (growable (array)
                            (live array "A growable value")
                            (let ((stack (datum-stack array)))
                              (when (and stacks stack (not (eq array (first (stack-items (stack stack))))))
                                (error "A growable value on stack ~d grew under another value" stack)))
                            (datum-fields array))
                          (view (value)
                            "The elements VALUE views, and the range of them."
                            (etypecase value
                              (storage (live value "An array read")
                               (values (datum-fields value) 0 (length (datum-fields value))))
                              (slice-value (live value "A slice read")
                               (values (datum-fields (slice-value-target value))
                                       (slice-value-start value) (slice-value-end value)))))
                          (vector-position (vector index)
                            (let ((fields (datum-fields vector)))
                              (unless (< -1 index (length fields))
                                (error "The index ~d is outside the vector of ~d elements" index (length fields)))
                              index))
                          (position-in (value index)
                            (multiple-value-bind (vector start end) (view value)
                              (unless (< -1 index (- end start))
                                (error "The index ~d is outside the array of ~d elements" index (- end start)))
                              (values vector (+ start index)))))
                   (ecase op
                     (:fixed-new (allocate :fixed (elements (fixed-length type) (first args)) node))
                     (:array-new (allocate :variable (elements (first args) (second args)) node))
                     (:cell-new (allocate :fixed (elements 1 (first args)) node))
                     (:grow-new (allocate :grow (elements 0 nil) node))
                     ((:stack-new :vector-new) (allocate :grow-shrink (elements 0 nil) node))
                     ((:grow-push :stack-push)
                      (vector-push-extend (second args) (growable (first args)))
                      (unit))
                     (:vector-push
                      (vector-push-extend (first args) (growable (second args))))
                     ((:stack-pop :vector-pop)
                      (let ((vector (growable (first args))))
                        (if (zerop (length vector))
                            (make-datum (constructor-named lowered type "NONE") '())
                            (make-datum (constructor-named lowered type "SOME") (list (vector-pop vector))))))
                     (:vector-index
                      (live (second args) "A vector read")
                      (let ((vector (datum-fields (second args))))
                        (if (< (first args) (length vector))
                            (make-datum (constructor-named lowered type "SOME") (list (aref vector (first args))))
                            (make-datum (constructor-named lowered type "NONE") '()))))
                     (:vector-index-unsafe
                      (live (second args) "A vector read")
                      (aref (datum-fields (second args)) (vector-position (second args) (first args))))
                     (:vector-set
                      (live (third args) "A vector written")
                      (setf (aref (datum-fields (third args)) (vector-position (third args) (first args)))
                            (second args))
                      (unit))
                     (:vector-length
                      (live (first args) "A vector read")
                      (length (datum-fields (first args))))
                     ((:hashtable-new :hashtable-with-capacity)
                      (allocate :grow-shrink (make-hash-table :test 'eql) node))
                     (:hashtable-set
                      (setf (gethash (second args) (growable (first args))) (third args))
                      (unit))
                     (:hashtable-get
                      (live (first args) "A hashtable read")
                      (multiple-value-bind (value found) (gethash (second args) (datum-fields (first args)))
                        (if found
                            (make-datum (constructor-named lowered type "SOME") (list value))
                            (make-datum (constructor-named lowered type "NONE") '()))))
                     (:hashtable-remove
                      (live (first args) "A hashtable written")
                      (remhash (second args) (datum-fields (first args)))
                      (unit))
                     (:hashtable-count
                      (live (first args) "A hashtable read")
                      (hash-table-count (datum-fields (first args))))
                     (:elements-length
                      (multiple-value-bind (vector start end) (view (first args))
                        (declare (ignore vector))
                        (- end start)))
                     (:elements-index
                      (multiple-value-bind (vector position) (position-in (first args) (second args))
                        (aref vector position)))
                     (:elements-set
                      (multiple-value-bind (vector position) (position-in (first args) (second args))
                        (setf (aref vector position) (third args))
                        (unit)))
                     (:elements-ref
                      (multiple-value-bind (vector position) (position-in (first args) (second args))
                        (declare (ignore vector))
                        (make-ref-value (if (typep (first args) 'storage)
                                            (first args)
                                            (slice-value-target (first args)))
                                        position)))
                     (:elements-slice
                      (destructuring-bind (source lo hi) args
                        (multiple-value-bind (vector start end) (view source)
                          (declare (ignore vector))
                          (unless (<= 0 lo hi (- end start))
                            (error "The range ~d to ~d is outside the array of ~d elements" lo hi (- end start)))
                          (make-slice-value (if (typep source 'storage) source (slice-value-target source))
                                            (+ start lo) (+ start hi)))))
                     (:cell-read
                      (live (first args) "A cell read")
                      (aref (datum-fields (first args)) 0))
                     (:cell-write
                      (live (first args) "A cell written")
                      (setf (aref (datum-fields (first args)) 0) (second args)))
                     (:cell-swap
                      (live (first args) "A cell written")
                      (shiftf (aref (datum-fields (first args)) 0) (second args)))
                     (:ref-get
                      (let ((ref (first args)))
                        (live ref "A reference read")
                        (aref (datum-fields (ref-value-target ref)) (ref-value-index ref))))
                     (:ref-set
                      (let ((ref (first args)))
                        (live ref "A reference written through")
                        (setf (aref (datum-fields (ref-value-target ref)) (ref-value-index ref))
                              (second args))
                        (unit)))))))
             (enter (def env)
               (let ((pool (ir-def-pool def)))
                 (when pool (claim pool))
                 (let ((value (evaluate (ir-def-body def) env)))
                   (when pool (release pool))
                   (live value (format nil "~s returned" (ir-def-name def))))))
             (force (name)
               (multiple-value-bind (value found) (gethash name globals)
                 (if found
                     value
                     (setf (gethash name globals) (enter (global name) '())))))
             (evaluate (node env)
               (etypecase node
                 (ir-literal (ir-literal-value node))
                 (ir-local (cdr (assoc (ir-local-name node) env)))
                 (ir-global (force (ir-global-name node)))
                 (ir-let
                  (let ((stacks (ir-let-stacks node)))
                    (mapc #'claim stacks)
                    (prog1 (evaluate (ir-let-body node)
                                     (acons (ir-let-name node) (evaluate (ir-let-value node) env) env))
                      (mapc #'release stacks))))
                 (ir-call
                  (let* ((def (global (ir-call-function node)))
                         (env (loop :for (name) :in (ir-def-params def)
                                    :for arg :in (ir-call-args node)
                                    :collect (cons name (evaluate arg env))))
                         (region (ir-call-region node)))
                    (when region (claim region))
                    (prog1 (enter def env)
                      (when region (release region)))))
                 (ir-construct
                  (let* ((args (mapcar (lambda (arg) (evaluate arg env)) (ir-construct-args node)))
                         (stack (ir-construct-stack node))
                         (cell (make-datum (ir-construct-constructor node) args stack)))
                    (dolist (arg args)
                      (live arg "A constructor stored"))
                    (when stack (push-cell stack cell))
                    cell))
                 (ir-prim
                  (let ((args (mapcar (lambda (arg) (evaluate arg env)) (ir-prim-args node))))
                    (if (prelude-op-p (ir-prim-op node))
                        (prelude (ir-prim-op node) args node)
                        (apply-primitive lowered (ir-prim-op node) args (ir-node-type node)))))
                 (ir-foreign (error "Raw Lisp is not evaluated: ~s" (ir-foreign-form node)))
                 (ir-match
                  (let ((value (live (evaluate (ir-match-scrutinee node) env) "A match read")))
                    (dolist (branch (ir-match-branches node) (error "No branch matches ~s" value))
                      (multiple-value-bind (matched branch-env)
                          (match-pattern (ir-branch-pattern branch) value env)
                        (when matched
                          (return (evaluate (ir-branch-body branch) branch-env))))))))))
      (let ((root (global (lowered-root lowered))))
        (when stacks
          (maphash (lambda (name def)
                     (declare (ignore name))
                     (when (or (eq (ir-def-kind def) :value) (eq def root))
                       (when (ir-def-destination def)
                         (claim (ir-def-destination def)))
                       (mapc #'claim (ir-def-growables def))))
                   defs)
          (maphash (lambda (name def)
                     (when (eq (ir-def-kind def) :value) (force name)))
                   defs))
        (values (if (eq (ir-def-kind root) :value)
                    (force (lowered-root lowered))
                    (enter root '()))
                (and stacks touched))))))
