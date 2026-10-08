;;;; defunctionalize.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defvar *defunctionalized* (make-hash-table :test 'equal))

(defun defunctionalized (fixture)
  "FIXTURE's monomorphized program defunctionalized, once."
  (or (gethash fixture *defunctionalized*)
      (setf (gethash fixture *defunctionalized*)
            (sheaf:defunctionalize (specialized fixture)))))

(defun count-nodes (specialized predicate)
  (let ((count 0))
    (maphash (lambda (name node)
               (declare (ignore name))
               (traverse:traverse
                node
                (list (traverse:action (:after ast:node n)
                        (when (funcall predicate n) (incf count))
                        (values)))))
             (sheaf:specialized-bindings specialized))
    count))

(defun union-match-p (node)
  "Whether NODE is a match on a synthesized union."
  (and (typep node 'ast:node-match)
       (some (lambda (branch)
               (let ((pattern (ast:match-branch-pattern branch)))
                 (and (typep pattern 'pattern:pattern-constructor)
                      (eq (find-package '#:sheaf/generated)
                          (symbol-package (pattern:pattern-constructor-name pattern))))))
             (ast:node-match-branches node))))

(defun synthesized-type (specialized prefix)
  "The type entries of SPECIALIZED's synthesized types whose names start with PREFIX."
  (loop :for definition :in (sheaf:specialized-type-definitions specialized)
        :for name := (tc:tycon-name (coalton-impl/typechecker/define-type:type-definition-type definition))
        :when (eql 0 (search prefix (symbol-name name)))
          :collect (tc:lookup-type (sheaf:specialized-environment specialized) name)))

(defun root-result-type (specialized binding)
  "The result type of the specialization of BINDING."
  (loop :for name :being :the :hash-keys :of (sheaf:specialized-bindings specialized)
          :using (:hash-value node)
        :when (eq binding (sheaf:specialized-origin specialized name))
          :return (tc:function-return-type (ast:node-type node))))

(test defunctionalized-programs-are-first-order
  (dolist (fixture '("counter" "closures" "nested"))
    (is-true (sheaf:check-defunctionalized (defunctionalized fixture)) "~a" fixture)))

(test defunctionalized-programs-run
  (is (= 287 (run-specialized (defunctionalized "counter"))))
  (is (= 51 (run-specialized (defunctionalized "closures"))))
  (is (= (run-specialized (specialized "nested"))
         (run-specialized (defunctionalized "nested")))))

(test lambdas-reaching-one-slot-become-cases
  (let* ((result (defunctionalized "closures"))
         (union (root-result-type result (fixture-symbol "closures" "PICK")))
         (entry (tc:lookup-type (sheaf:specialized-environment result) (tc:tycon-name union)))
         (arities (sort (mapcar (lambda (constructor)
                                  (tc:constructor-entry-arity
                                   (tc:lookup-constructor (sheaf:specialized-environment result) constructor)))
                                (tc:type-entry-constructors entry))
                        #'<)))
    (is (equal '(0 1) arities))))

(test calls-dispatch-on-the-tag
  (let ((result (defunctionalized "closures")))
    (is (plusp (count-nodes result #'union-match-p)))
    (is (zerop (count-nodes (defunctionalized "counter")
                            (lambda (node)
                              (and (union-match-p node)
                                   (null (rest (ast:node-match-branches node)))
                                   nil)))))))

(test a-lone-lambda-is-called-directly
  (let ((result (defunctionalized "closures")))
    (maphash (lambda (name node)
               (when (eq (fixture-symbol "closures" "SUM-TO") (sheaf:specialized-origin result name))
                 (is (zerop (count-nodes-in node #'union-match-p)))))
             (sheaf:specialized-bindings result))))

(defun count-nodes-in (root predicate)
  (let ((count 0))
    (traverse:traverse root (list (traverse:action (:after ast:node n)
                                    (when (funcall predicate n) (incf count))
                                    (values))))
    count))

(test higher-order-function-specialized-per-lambda-set
  (is (= 2 (length (specializations-of (defunctionalized "closures")
                                       (fixture-symbol "closures" "APPLY-TWICE"))))))

(test iterators-with-different-closures-are-different-types
  (is (= 2 (length (synthesized-type (defunctionalized "counter") "ITERATOR")))))

(test recursive-local-function-needs-no-recursive-type
  (is (null (sheaf:recursive-types (defunctionalized "closures"))))
  (is (null (sheaf:recursive-types (defunctionalized "counter")))))

(test nesting-to-a-runtime-depth-makes-a-recursive-type
  (is-true (sheaf:recursive-types (defunctionalized "nested"))))

(test a-function-reaching-raw-lisp-is-an-error
  (let ((chain (handler-case (progn (sheaf:defunctionalize (specialized "escape")) nil)
                 (sheaf:specialization-error (e) (sheaf:specialization-error-chain e)))))
    (is-true chain)
    (is-true (third (first chain)))))
