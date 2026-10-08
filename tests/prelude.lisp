;;;; prelude.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defun prims (lowered)
  (let ((nodes '()))
    (maphash (lambda (name def)
               (declare (ignore name))
               (sheaf:walk-ir (lambda (node) (when (typep node 'sheaf:ir-prim) (push node nodes)))
                              (sheaf:ir-def-body def)))
             (sheaf:lowered-defs lowered))
    nodes))

(defun allocating-prims (lowered)
  (remove-if-not (lambda (node) (sheaf::primitive-allocates (sheaf:ir-prim-op node)))
                 (prims lowered)))

(defun run-on-sbcl (fixture)
  (fixture-program fixture)
  (funcall (symbol-function (fixture-symbol fixture "MAIN"))))

(test arrays-give-one-result-on-every-tier
  (let ((expected (run-on-sbcl "arrays")))
    (is (= 116 expected))
    (is (= expected (sheaf:run-lowered (lowered "arrays"))))
    (is (= expected (sheaf:run-lowered (assigned "arrays"))))))

(test arrays-lower-to-primitives
  (is (zerop (count-ir (lowered "arrays") 'sheaf:ir-foreign)))
  (is (equal (sort (mapcar #'first sheaf::*prelude-ops*) #'string<)
             (sort (remove-duplicates (mapcar #'sheaf:ir-prim-op
                                              (remove-if (lambda (node)
                                                           (member (sheaf:ir-prim-op node)
                                                                   '(:ufix-add :ufix-sub :ufix-mul :ufix-eq
                                                                     :ufix-compare :ufix-from-int :ordering-eq)))
                                                         (prims (lowered "arrays")))))
                   #'string<))))

(test arrays-are-well-formed
  (is-true (sheaf:check-lowered (lowered "arrays")))
  (is-true (sheaf:check-assigned (assigned "arrays")))
  (is-true (sheaf:check-lowered (assigned "arrays"))))

(test every-allocation-has-a-stack
  (let ((assigned (assigned "arrays")))
    (is (plusp (length (allocating-prims assigned))))
    (dolist (node (allocating-prims assigned))
      (is (< -1 (sheaf:ir-prim-stack node) (sheaf:lowered-stack-count assigned))))))

(test a-growable-local-owns-its-stack
  (let ((grown (first (clones "arrays" "GROWN"))))
    (sheaf:walk-ir (lambda (node)
                     (when (and (typep node 'sheaf:ir-let)
                                (typep (sheaf:ir-let-value node) 'sheaf:ir-prim)
                                (eq :grow-new (sheaf:ir-prim-op (sheaf:ir-let-value node))))
                       (is (integerp (sheaf:ir-let-stack node)))
                       (is (= (sheaf:ir-let-stack node)
                              (sheaf:ir-prim-stack (sheaf:ir-let-value node))))))
                   (sheaf:ir-def-body grown))))

(test a-fixed-array-reads-its-length-from-its-type
  (let ((node (find :fixed-new (prims (lowered "arrays")) :key #'sheaf:ir-prim-op)))
    (is (= 4 (sheaf::fixed-length (sheaf:ir-node-type node))))))

(test an-undeclared-length-is-an-error
  (signals sheaf:specialization-error (lowered "bad-length")))

(test an-index-past-the-end-is-an-error
  (signals error (sheaf:run-lowered (lowered "out-of-bounds"))))

(defun with-shared-stack (fixture name)
  "A fresh assignment of FIXTURE in which every array of the function NAME is
pushed to that function's pool."
  (let ((assigned (sheaf:assign-stacks (lowered fixture))))
    (dolist (def (clones-of assigned fixture name))
      (sheaf:walk-ir (lambda (node)
                       (when (and (typep node 'sheaf:ir-prim)
                                  (sheaf::primitive-allocates (sheaf:ir-prim-op node)))
                         (setf (sheaf::ir-prim-stack node) (sheaf:ir-def-pool def))))
                     (sheaf:ir-def-body def)))
    assigned))

(defun clones-of (assigned fixture name)
  (loop :for def :being :the :hash-values :of (sheaf:lowered-defs assigned)
        :when (eq (sheaf:ir-def-origin def) (fixture-symbol fixture name))
          :collect def))

(test a-growable-under-another-value-is-caught
  (is (integerp (sheaf:ir-def-pool (first (clones "arrays" "TWO-ARRAYS")))))
  (signals error (sheaf:run-lowered (with-shared-stack "arrays" "TWO-ARRAYS"))))

(test arrays-are-not-emitted
  (let ((message (handler-case (progn (sheaf:emit-c (assigned "arrays")) "")
                   (error (condition) (princ-to-string condition)))))
    (is (search "sheaf/27" message))))
