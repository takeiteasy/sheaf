;;;; interpret.lisp
;;;; A reference interpreter for the IR, which defines what a lowered program means.

(in-package #:sheaf)

(defstruct (datum (:constructor make-datum (constructor fields &optional stack)))
  "A value of a data type. STACK is the data stack holding it, if it is a cell."
  constructor fields stack (freed nil))

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
               (when (and (datum-p value) (datum-freed value))
                 (error "~a a cell on stack ~d after it was freed" what (datum-stack value)))
               value)
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
                  (let ((stack (ir-let-stack node)))
                    (when stack (claim stack))
                    (prog1 (evaluate (ir-let-body node)
                                     (acons (ir-let-name node) (evaluate (ir-let-value node) env) env))
                      (when stack (release stack)))))
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
                  (apply-primitive lowered (ir-prim-op node)
                                   (mapcar (lambda (arg) (evaluate arg env)) (ir-prim-args node))
                                   (ir-node-type node)))
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
                     (when (and (ir-def-destination def)
                                (or (eq (ir-def-kind def) :value) (eq def root)))
                       (claim (ir-def-destination def))))
                   defs)
          (maphash (lambda (name def)
                     (when (eq (ir-def-kind def) :value) (force name)))
                   defs))
        (values (if (eq (ir-def-kind root) :value)
                    (force (lowered-root lowered))
                    (enter root '()))
                (and stacks touched))))))
