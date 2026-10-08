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
  (dolist (fixture '("counter" "closures" "flows" "recursive-data"))
    (is-true (sheaf:check-defunctionalized (defunctionalized fixture)) "~a" fixture)))

(test defunctionalized-programs-run
  (is (= 287 (run-specialized (defunctionalized "counter"))))
  (is (= 51 (run-specialized (defunctionalized "closures"))))
  (is (= 55 (run-specialized (defunctionalized "flows"))))
  (is (= (run-specialized (specialized "recursive-data"))
         (run-specialized (defunctionalized "recursive-data")))))

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

(defun branch-count-p (count)
  (lambda (node)
    (and (union-match-p node) (= count (length (ast:node-match-branches node))))))

(test calls-dispatch-on-the-tag
  (is (plusp (count-nodes (defunctionalized "closures") (branch-count-p 2)))))

(defun lifted-bindings (specialized name)
  "The nodes of the specializations of the global NAME."
  (loop :for symbol :being :the :hash-keys :of (sheaf:specialized-bindings specialized)
          :using (:hash-value node)
        :when (eq name (sheaf:specialized-origin specialized symbol))
          :collect node))

(test a-lone-lambda-is-called-directly
  (let ((nodes (lifted-bindings (defunctionalized "closures") (fixture-symbol "closures" "SUM-TO"))))
    (is (plusp (length nodes)))
    (dolist (node nodes)
      (is (zerop (count-nodes-in node #'union-match-p))))))

(defun count-nodes-in (root predicate)
  (let ((count 0))
    (traverse:traverse root (list (traverse:action (:after ast:node n)
                                    (when (funcall predicate n) (incf count))
                                    (values))))
    count))

(test a-function-as-a-type-argument-keeps-its-type
  (let ((types (specializations-of (defunctionalized "flows") (fixture-symbol "flows" "USE-OPTION"))))
    (is (= 2 (length types)))
    (is (every (lambda (type) (search "OPTIONAL FN" type)) types))))

(test a-call-no-lambda-reaches-is-dead-code
  (let ((nodes (lifted-bindings (defunctionalized "flows") (fixture-symbol "flows" "USE-OPTION"))))
    (is (some (lambda (node) (plusp (count-nodes-in node (lambda (n)
                                                           (and (typep n 'ast:node-match)
                                                                (null (ast:node-match-branches n)))))))
              nodes))))

(test higher-order-function-specialized-per-lambda-set
  (is (= 2 (length (specializations-of (defunctionalized "closures")
                                       (fixture-symbol "closures" "APPLY-TWICE"))))))

(test iterators-with-different-closures-are-different-types
  (is (= 2 (length (synthesized-type (defunctionalized "counter") "ITERATOR")))))

(test recursive-local-function-needs-no-recursive-type
  (is (null (sheaf:recursive-types (defunctionalized "closures"))))
  (is (null (sheaf:recursive-types (defunctionalized "counter")))))

(defun defunctionalize-error-chain (fixture)
  (handler-case (progn (sheaf:defunctionalize (specialized fixture)) nil)
    (sheaf:specialization-error (e) (sheaf:specialization-error-chain e))))

(test nesting-to-a-runtime-depth-is-an-error
  (loop :for (fixture binding) :in '(("nested" "REWRAP") ("nested-optional" "CHAIN"))
        :do (let ((entry (first (defunctionalize-error-chain fixture))))
              (is-true entry "~a" fixture)
              (is-true (search binding (symbol-name (first entry))) "~a" fixture)
              (is-true (third entry) "~a" fixture))))

(test a-declared-recursive-type-holding-functions-is-allowed
  (let ((result (defunctionalized "recursive-data")))
    (is-true (sheaf:recursive-types result))))

(test a-function-reaching-raw-lisp-is-an-error
  (let ((chain (handler-case (progn (sheaf:defunctionalize (specialized "escape")) nil)
                 (sheaf:specialization-error (e) (sheaf:specialization-error-chain e)))))
    (is-true chain)
    (is-true (third (first chain)))))

(test equal-cyclic-types-key-equally
  ;; One list node whose tail is itself, and one that reaches it through a prefix.
  (let* ((lam (sheaf::make-lam 0 nil nil nil))
         (ty (tc:make-tycon :name 'coalton:ufix :kind tc:+kstar+))
         (tight (sheaf::make-fn-ctype))
         (prefix (sheaf::make-fn-ctype))
         (other (sheaf::make-fn-ctype)))
    (setf (sheaf::fn-ctype-cases tight) (list (list lam tight (sheaf::make-atom-ctype ty)))
          (sheaf::fn-ctype-cases other) (list (list lam prefix (sheaf::make-atom-ctype ty)))
          (sheaf::fn-ctype-cases prefix) (list (list lam other (sheaf::make-atom-ctype ty))))
    (is (equal (sheaf::ctype-key tight nil) (sheaf::ctype-key other nil)))
    (is (not (equal (sheaf::ctype-key tight nil)
                    (sheaf::ctype-key (let ((c (sheaf::make-fn-ctype)))
                                        (setf (sheaf::fn-ctype-cases c) (list (list lam (sheaf::make-atom-ctype ty))))
                                        c)
                                      nil))))))
