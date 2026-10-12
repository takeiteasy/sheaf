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

(defun allocating-prims-of (def)
  (let ((nodes '()))
    (sheaf:walk-ir (lambda (node)
                     (when (and (typep node 'sheaf:ir-prim) (sheaf::primitive-allocates (sheaf:ir-prim-op node)))
                       (push node nodes)))
                   (sheaf:ir-def-body def))
    nodes))

(defun run-on-sbcl (fixture)
  (fixture-program fixture)
  (funcall (symbol-function (fixture-symbol fixture "MAIN"))))

(test arrays-give-one-result-on-every-tier
  (let ((expected (run-on-sbcl "arrays")))
    (is (= 116 expected))
    (is (= expected (sheaf:run-lowered (lowered "arrays"))))
    (is (= expected (sheaf:run-lowered (assigned "arrays"))))))

(defun prelude-ops-used (&rest fixtures)
  "The prelude operations the fixtures' mains, and the hashtables' COUNTED, use."
  (sort (remove-duplicates
         (loop :for fixture :in fixtures
               :append (mapcar #'sheaf:ir-prim-op (prims-with-prelude-ops (lowered fixture)))
               :when (string= fixture "hashtables")
                 :append (mapcar #'sheaf:ir-prim-op
                                 (prims-with-prelude-ops (lowered-from fixture "COUNTED")))))
        #'string<))

(test arrays-lower-to-primitives
  (is (zerop (count-ir (lowered "arrays") 'sheaf:ir-foreign)))
  (is (zerop (count-ir (lowered "cells") 'sheaf:ir-foreign)))
  (is (zerop (count-ir (lowered "vectors") 'sheaf:ir-foreign)))
  (is (zerop (count-ir (lowered "hashtables") 'sheaf:ir-foreign)))
  (is (equal (sort (mapcar #'first sheaf::*prelude-ops*) #'string<)
             (prelude-ops-used "arrays" "cells" "vectors" "hashtables" "vector-library" "lists" "table-iterators"))))

(test standard-library-functions-run-on-every-tier
  (let ((expected (run-on-sbcl "stdlib")))
    (is (= 19 expected))
    (is (zerop (count-ir (lowered "stdlib") 'sheaf:ir-foreign)))
    (is (= expected (sheaf:run-lowered (lowered "stdlib"))))
    (is (= expected (sheaf:run-lowered (assigned "stdlib"))))))

(test a-function-returning-void-runs-on-every-tier
  (let ((expected (run-on-sbcl "voids")))
    (is (= 5 expected))
    (is (zerop (count-ir (lowered "voids") 'sheaf:ir-foreign)))
    (is (= expected (sheaf:run-lowered (lowered "voids"))))
    (is (= expected (sheaf:run-lowered (assigned "voids"))))))

(test an-early-return-runs-on-every-tier
  (let ((expected (run-on-sbcl "returns")))
    (is (= 224 expected))
    (is (zerop (count-ir (lowered "returns") 'sheaf:ir-foreign)))
    (is (plusp (count-ir (lowered "returns") 'sheaf:ir-return)))
    (is (= expected (sheaf:run-lowered (lowered "returns"))))
    (is (= expected (sheaf:run-lowered (assigned "returns"))))))

(test a-return-out-of-a-catch-is-an-error
  (is (search "issues/40" (error-message (lambda () (lowered-from "returns-rejected" "MAIN-CATCH"))))))

(test an-early-return-of-a-growable-value-runs-on-every-tier
  (let ((expected (run-on-sbcl "growable-returns")))
    (is (zerop (count-ir (lowered "growable-returns") 'sheaf:ir-foreign)))
    (is (plusp (count-ir (lowered "growable-returns") 'sheaf:ir-return)))
    (is (= expected (sheaf:run-lowered (lowered "growable-returns"))))
    (is (= expected (sheaf:run-lowered (assigned "growable-returns"))))))

(test an-early-return-below-the-body-runs-on-every-tier
  (dolist (name '("GROWN" "BRANCHY"))
    (dolist (crossing '(nil t))
      (loop :for (n expected) :in '((9 1) (2 2))
            :do (let ((program (below-the-body name n :crossing crossing)))
                  (is (hash-table-p (sheaf:check-roots program)))
                  (is (= expected (sheaf:run-lowered program)))
                  (is (= expected (sheaf:run-lowered (sheaf:assign-stacks program)))))))))

(test a-loop-gives-back-the-growable-values-its-body-builds-at-each-back-edge
  (is (some (lambda (def)
              (let ((found nil))
                (sheaf:walk-ir (lambda (node)
                                 (when (and (sheaf::ir-loop-p node) (sheaf::ir-loop-reset node))
                                   (setf found t)))
                               (sheaf:ir-def-body def))
                found))
            (loop :for def :being :the :hash-values :of (sheaf:lowered-defs (assigned "growable-returns"))
                  :when (search "FOUND" (symbol-name (sheaf:ir-def-name def))) :collect def))))

(test loops-run-on-every-tier
  (let ((expected (run-on-sbcl "for-loops")))
    (is (= 500 expected))
    (is (zerop (count-ir (lowered "for-loops") 'sheaf:ir-foreign)))
    (is (plusp (count-ir (lowered "for-loops") 'sheaf:ir-loop)))
    (is (= expected (sheaf:run-lowered (lowered "for-loops"))))
    (is (= expected (sheaf:run-lowered (assigned "for-loops"))))))

(test a-break-out-of-a-catch-or-a-rebuilt-growable-variable-is-an-error
  (is (search "issues/40" (error-message (lambda () (lowered-from "for-rejected" "MAIN-BREAK")))))
  (is (search "issues/41" (error-message
                           (lambda ()
                             (sheaf:assign-stacks (lowered-from "for-rejected" "MAIN-REBUILT")))))))

(defun loops-of (lowered)
  "The loops in LOWERED."
  (let ((loops '()))
    (maphash (lambda (name def)
               (declare (ignore name))
               (sheaf:walk-ir (lambda (node) (when (typep node 'sheaf:ir-loop) (push node loops)))
                              (sheaf:ir-def-body def)))
             (sheaf:lowered-defs lowered))
    loops))

(test a-loop-that-gives-back-its-cells-runs-on-every-tier
  (let ((expected (run-on-sbcl "loop-reuse")))
    (is (= 200001 expected))
    (is (= expected (sheaf:run-lowered (lowered "loop-reuse"))))
    (is (= expected (sheaf:run-lowered (assigned "loop-reuse"))))
    (is (= 1 (count-if #'sheaf:ir-loop-reset (loops-of (assigned "loop-reuse")))))))

(test vector-functions-run-on-every-tier
  (let ((expected (run-on-sbcl "vector-library")))
    (is (zerop (count-ir (lowered "vector-library") 'sheaf:ir-foreign)))
    (is (= expected (sheaf:run-lowered (lowered "vector-library"))))
    (is (= expected (sheaf:run-lowered (assigned "vector-library"))))))

(test vector-folds-sorts-and-searches-run-on-every-tier
  (let ((expected (run-on-sbcl "vector-folds")))
    (is (zerop (count-ir (lowered "vector-folds") 'sheaf:ir-foreign)))
    (is (= expected (sheaf:run-lowered (lowered "vector-folds"))))
    (is (= expected (sheaf:run-lowered (assigned "vector-folds"))))))

(test vectors-and-hashtables-filled-from-an-iterator-run-on-every-tier
  (let ((expected (run-on-sbcl "extend")))
    (is (zerop (count-ir (lowered "extend") 'sheaf:ir-foreign)))
    (is (= expected (sheaf:run-lowered (lowered "extend"))))
    (is (= expected (sheaf:run-lowered (assigned "extend"))))))

(test hashtable-iterators-run-on-every-tier
  (let ((expected (run-on-sbcl "table-iterators")))
    (is (zerop (count-ir (lowered "table-iterators") 'sheaf:ir-foreign)))
    (is (= expected (sheaf:run-lowered (lowered "table-iterators"))))
    (is (= expected (sheaf:run-lowered (assigned "table-iterators"))))))

(test list-functions-run-on-every-tier
  (dolist (fixture '("lists" "poly"))
    (let ((expected (run-on-sbcl fixture)))
      (is (zerop (count-ir (lowered fixture) 'sheaf:ir-foreign)) "~a" fixture)
      (is (= expected (sheaf:run-lowered (lowered fixture))) "~a" fixture)
      (is (= expected (sheaf:run-lowered (assigned fixture))) "~a" fixture))))

(test cells-give-one-result-on-every-tier
  (let ((expected (run-on-sbcl "cells")))
    (is (= 29 expected))
    (is (= expected (sheaf:run-lowered (lowered "cells"))))
    (is (= expected (sheaf:run-lowered (assigned "cells"))))))

(test vectors-give-one-result-on-every-tier
  (let ((expected (run-on-sbcl "vectors")))
    (is (= 53 expected))
    (is (= expected (sheaf:run-lowered (lowered "vectors"))))
    (is (= expected (sheaf:run-lowered (assigned "vectors"))))))

(test vectors-are-well-formed
  (is-true (sheaf:check-lowered (lowered "vectors"))))

(test a-returned-vector-is-built-on-a-growable-stack-of-the-caller
  (let ((clone (first (clones "vectors" "FILLED"))))
    (is (null (sheaf:ir-def-destination clone)))
    (is (= 1 (length (sheaf:ir-def-growables clone))))
    (dolist (node (allocating-prims-of clone))
      (is (eql (first (sheaf:ir-def-growables clone)) (sheaf:ir-prim-stack node))))))

(test a-vector-local-owns-its-stack
  (let ((total (first (clones "vectors" "LOCAL-TOTAL"))) (seen 0))
    (sheaf:walk-ir (lambda (node)
                     (when (and (typep node 'sheaf:ir-let)
                                (typep (sheaf:ir-let-value node) 'sheaf:ir-prim)
                                (eq :vector-new (sheaf:ir-prim-op (sheaf:ir-let-value node))))
                       (incf seen)
                       (is (equal (list (sheaf:ir-prim-stack (sheaf:ir-let-value node)))
                                  (sheaf:ir-let-stacks node)))))
                   (sheaf:ir-def-body total))
    (is (= 1 seen))))

(defun lowered-from (fixture root)
  "FIXTURE's function ROOT, lowered."
  (sheaf:lower (sheaf:defunctionalize
                (sheaf:monomorphize (fixture-program fixture) (fixture-symbol fixture root)))))

(test hashtables-give-one-result-on-every-tier
  (let ((expected (run-on-sbcl "hashtables")))
    (is (= 90 expected))
    (is (= expected (sheaf:run-lowered (lowered "hashtables"))))
    (is (= expected (sheaf:run-lowered (assigned "hashtables"))))))

(test a-hashtable-count-gives-one-result-on-every-tier
  (let ((lowered (lowered-from "hashtables" "COUNTED")))
    (is (= 2 (funcall (symbol-function (fixture-symbol "hashtables" "COUNTED")))))
    (is (= 2 (sheaf:run-lowered lowered)))
    (is (= 2 (sheaf:run-lowered (sheaf:assign-stacks lowered))))))

(test hashtables-are-well-formed
  (is-true (sheaf:check-lowered (lowered "hashtables"))))

(test a-returned-hashtable-is-built-on-a-growable-stack-of-the-caller
  (let ((clone (first (clones "hashtables" "FILLED"))))
    (is (null (sheaf:ir-def-destination clone)))
    (is (= 1 (length (sheaf:ir-def-growables clone))))
    (dolist (node (allocating-prims-of clone))
      (is (eql (first (sheaf:ir-def-growables clone)) (sheaf:ir-prim-stack node))))))

(test a-hashtable-local-owns-its-stack
  (let ((total (first (clones "hashtables" "LOCAL-TOTAL"))) (seen 0))
    (sheaf:walk-ir (lambda (node)
                     (when (and (typep node 'sheaf:ir-let)
                                (typep (sheaf:ir-let-value node) 'sheaf:ir-prim)
                                (eq :hashtable-new (sheaf:ir-prim-op (sheaf:ir-let-value node))))
                       (incf seen)
                       (is (equal (list (sheaf:ir-prim-stack (sheaf:ir-let-value node)))
                                  (sheaf:ir-let-stacks node)))))
                   (sheaf:ir-def-body total))
    (is (= 1 seen))))

(test a-hashtable-keyed-by-another-type-is-rejected
  (let ((condition (handler-case (lowered-from "hashtables" "BOOLEAN-KEYS")
                     (sheaf:specialization-error (condition) condition))))
    (is (typep condition 'sheaf:specialization-error))
    (is (search "/sheaf/issues/20" (sheaf:specialization-error-message condition)))))

(test a-returned-cell-is-built-in-the-destination
  (let ((clone (first (clones "cells" "FRESH-CELL"))))
    (is (integerp (sheaf:ir-def-destination clone)))
    (dolist (node (allocating-prims-of clone))
      (is (eql (sheaf:ir-def-destination clone) (sheaf:ir-prim-stack node))))))

(test a-cell-a-returned-closure-holds-is-built-in-the-destination
  (let ((clone (first (clones "cells" "MAKE-COUNTER"))))
    (is (integerp (sheaf:ir-def-destination clone)))
    (dolist (node (allocating-prims-of clone))
      (is (eql (sheaf:ir-def-destination clone) (sheaf:ir-prim-stack node))))))

(test a-cell-that-is-dropped-is-built-in-a-pool
  (let ((clone (first (clones "cells" "LOCAL-SUM"))))
    (is (null (sheaf:ir-def-destination clone)))
    (is (integerp (sheaf:ir-def-pool clone)))
    (dolist (node (allocating-prims-of clone))
      (is (eql (sheaf:ir-def-pool clone) (sheaf:ir-prim-stack node))))))

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
                       (is (equal (list (sheaf:ir-prim-stack (sheaf:ir-let-value node)))
                                  (sheaf:ir-let-stacks node)))))
                   (sheaf:ir-def-body grown))))

(test a-fixed-array-reads-its-length-from-its-type
  (let ((node (find :fixed-new (prims (lowered "arrays")) :key #'sheaf:ir-prim-op)))
    (is (= 4 (sheaf::fixed-length (sheaf:ir-node-type node))))))

(test an-undeclared-length-is-an-error
  (signals sheaf:specialization-error (lowered "bad-length")))

(defun error-text (thunk)
  (handler-case (progn (funcall thunk) "")
    (error (condition) (princ-to-string condition))))

(test an-index-past-the-end-is-an-error
  (is (search "outside the array"
              (error-text (lambda () (sheaf:run-lowered (lowered "out-of-bounds")))))))

(test a-programs-own-instance-of-a-prelude-class-is-an-ordinary-function
  (is (= 6 (sheaf:run-lowered (lowered "own-indexed"))))
  (is (null (prims-with-prelude-ops (lowered "own-indexed")))))

(defun prims-with-prelude-ops (lowered)
  (remove-if-not (lambda (node) (sheaf::prelude-op-p (sheaf:ir-prim-op node))) (prims lowered)))

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
  (is (search "grew under"
              (error-text (lambda () (sheaf:run-lowered (with-shared-stack "arrays" "TWO-ARRAYS")))))))

(test a-generic-loop-runs-on-every-tier
  (let ((expected (run-on-sbcl "generic-loops")))
    (is (= 5 expected))
    (is (= expected (sheaf:run-lowered (lowered "generic-loops"))))
    (is (= expected (sheaf:run-lowered (assigned "generic-loops"))))))
