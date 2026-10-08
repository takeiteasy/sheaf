;;;; lower.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defvar *lowered* (make-hash-table :test 'equal))

(defun lowered (fixture)
  "FIXTURE's defunctionalized program lowered, once."
  (or (gethash fixture *lowered*)
      (setf (gethash fixture *lowered*)
            (sheaf:lower (defunctionalized fixture)))))

(defun count-ir (lowered type)
  (let ((count 0))
    (maphash (lambda (name def)
               (declare (ignore name))
               (sheaf:walk-ir (lambda (node) (when (typep node type) (incf count)))
                              (sheaf:ir-def-body def)))
             (sheaf:lowered-defs lowered))
    count))

(test lowered-programs-are-well-formed
  (dolist (fixture '("counter" "closures" "flows" "recursive-data"))
    (is-true (sheaf:check-lowered (lowered fixture)) "~a" fixture)))

(test lowered-programs-run
  (is (= 51 (sheaf:run-lowered (lowered "closures"))))
  (is (= 55 (sheaf:run-lowered (lowered "flows"))))
  (is (= 21 (sheaf:run-lowered (lowered "recursive-data")))))

(test operands-are-atoms
  (dolist (fixture '("counter" "closures" "flows" "recursive-data"))
    (maphash (lambda (name def)
               (declare (ignore name))
               (sheaf:walk-ir
                (lambda (node)
                  (dolist (operand (typecase node
                                     (sheaf:ir-call (sheaf:ir-call-args node))
                                     (sheaf:ir-construct (sheaf:ir-construct-args node))
                                     (sheaf:ir-prim (sheaf:ir-prim-args node))
                                     (sheaf:ir-match (list (sheaf:ir-match-scrutinee node)))))
                    (is (typep operand 'sheaf:ir-atom) "~a" fixture)))
                (sheaf:ir-def-body def)))
             (sheaf:lowered-defs (lowered fixture)))))

(test arithmetic-becomes-primitives
  (let ((lowered (lowered "closures")))
    (is (plusp (count-ir lowered 'sheaf:ir-prim)))
    (is-false (loop :for def :being :the :hash-values :of (sheaf:lowered-defs lowered)
                      :thereis (and (sheaf:ir-def-origin def)
                                    (search "INSTANCE/CLS:NUM" (symbol-name (sheaf:ir-def-origin def))))))))

(test literals-are-typed
  (let ((lowered (lowered "closures")))
    (maphash (lambda (name def)
               (declare (ignore name))
               (sheaf:walk-ir (lambda (node)
                                (when (typep node 'sheaf:ir-literal)
                                  (is (integerp (sheaf:ir-literal-value node)))
                                  (is (eq (find-symbol "UFIX" "COALTON")
                                          (sheaf:ir-type-name (sheaf:ir-node-type node))))))
                              (sheaf:ir-def-body def)))
             (sheaf:lowered-defs lowered))))

(test data-types-list-their-constructors
  (let ((unions (loop :for type :being :the :hash-keys :of (sheaf:lowered-types (lowered "closures"))
                        :using (:hash-value constructors)
                      :when (eql 0 (search "FN" (symbol-name (sheaf:ir-type-name type))))
                        :collect constructors)))
    (is (member '((0 . 1) (1 . 0))
                (mapcar (lambda (constructors)
                          (mapcar (lambda (constructor)
                                    (cons (sheaf:ir-constructor-tag constructor)
                                          (length (sheaf:ir-constructor-fields constructor))))
                                  constructors))
                        unions)
                :test #'equal))))

(test raw-lisp-stays-opaque
  (let ((lowered (lowered "counter")))
    (is (plusp (count-ir lowered 'sheaf:ir-foreign)))
    (signals error (sheaf:run-lowered lowered))))

(test locations-are-kept
  (let ((lowered (lowered "closures")) (missing 0))
    (maphash (lambda (name def)
               (declare (ignore name))
               (sheaf:walk-ir (lambda (node) (unless (sheaf:ir-node-location node) (incf missing)))
                              (sheaf:ir-def-body def)))
             (sheaf:lowered-defs lowered))
    (is (zerop missing))))
