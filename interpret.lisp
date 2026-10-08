;;;; interpret.lisp
;;;; A reference interpreter for the IR, which defines what a lowered program means.

(in-package #:sheaf)

(defstruct (datum (:constructor make-datum (constructor fields)))
  "A value of a data type."
  constructor fields)

(defun constructor-named (lowered type name)
  (ir-constructor-name
   (find name (lowered-constructors lowered type)
         :key (lambda (constructor) (symbol-name (ir-constructor-name constructor)))
         :test #'string=)))

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
cannot be evaluated: https://todo.sr.ht/~takeiteasy/sheaf/12"
  (let ((defs (lowered-defs lowered)))
    (labels ((global (name)
               (or (gethash name defs) (error "~s is not defined" name)))
             (evaluate (node env)
               (etypecase node
                 (ir-literal (ir-literal-value node))
                 (ir-local (cdr (assoc (ir-local-name node) env)))
                 (ir-global (evaluate (ir-def-body (global (ir-global-name node))) '()))
                 (ir-let (evaluate (ir-let-body node)
                                   (acons (ir-let-name node) (evaluate (ir-let-value node) env) env)))
                 (ir-call
                  (let ((def (global (ir-call-function node))))
                    (evaluate (ir-def-body def)
                              (loop :for (name) :in (ir-def-params def)
                                    :for arg :in (ir-call-args node)
                                    :collect (cons name (evaluate arg env))))))
                 (ir-construct
                  (make-datum (ir-construct-constructor node)
                              (mapcar (lambda (arg) (evaluate arg env)) (ir-construct-args node))))
                 (ir-prim
                  (apply-primitive lowered (ir-prim-op node)
                                   (mapcar (lambda (arg) (evaluate arg env)) (ir-prim-args node))
                                   (ir-node-type node)))
                 (ir-foreign (error "Raw Lisp is not evaluated: ~s" (ir-foreign-form node)))
                 (ir-match
                  (let ((value (evaluate (ir-match-scrutinee node) env)))
                    (dolist (branch (ir-match-branches node) (error "No branch matches ~s" value))
                      (multiple-value-bind (matched branch-env)
                          (match-pattern (ir-branch-pattern branch) value env)
                        (when matched
                          (return (evaluate (ir-branch-body branch) branch-env))))))))))
      (evaluate (ir-def-body (global (lowered-root lowered))) '()))))
