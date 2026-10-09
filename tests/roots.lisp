;;;; roots.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defun root-program (fixture root)
  "FIXTURE's function ROOT, lowered."
  (sheaf:lower (sheaf:defunctionalize
                (sheaf:monomorphize (fixture-program fixture) (fixture-symbol fixture root)))))

(defun root-error (root)
  "The error root checking signals for ROOT of the rejected fixture, or NIL."
  (handler-case (progn (sheaf:check-roots (root-program "roots-rejected" root)) nil)
    (sheaf:specialization-error (condition) condition)))

(defun chain-texts (condition)
  "The source text of each place in CONDITION's chain, which Coalton keeps in
upper case."
  (loop :for (nil nil location) :in (sheaf:specialization-error-chain condition)
        :collect (string-downcase (source:extract-source-text (source:location-source location)
                                                              (source:location-span location)))))

(defun summary (summaries name)
  "The summary of the function NAME of the roots fixture."
  (loop :for def :being :the :hash-values :of (sheaf:lowered-defs (lowered "roots"))
        :when (eq (sheaf:ir-def-origin def) (fixture-symbol "roots" name))
          :return (gethash (sheaf:ir-def-name def) summaries)))

(test accepted-references-run-on-every-tier
  (fixture-program "roots")
  (let ((expected (funcall (symbol-function (fixture-symbol "roots" "MAIN")))))
    (is (= 86 expected))
    (is (= expected (sheaf:run-lowered (lowered "roots"))))
    (is (= expected (sheaf:run-lowered (assigned "roots"))))))

(test accepted-references-are-accepted
  (is (hash-table-p (sheaf:check-roots (lowered "roots")))))

(test summaries-record-what-a-function-does-with-roots
  (let ((summaries (sheaf:check-roots (lowered "roots"))))
    (flet ((result (name) (sort (copy-list (sheaf:root-summary-result (summary summaries name)))
                                (lambda (a b) (string< (princ-to-string a) (princ-to-string b))))))
      (is (equal '(1) (result "WALK")))
      (is (equal '(0) (result "SECOND-REF")))
      (is (equal '(0) (result "PAIR-OF")))
      (is (equal '(1 2) (result "PICK")))
      (is (equal '(1 2) (result "PING")))
      (is (equal '(1 2) (result "PONG")))
      (is (equal '(:fresh) (result "FRESH-REF")))
      (is (equal '() (result "MAIN"))))
    (is (equal '((1 0)) (mapcar (lambda (store) (list (first store) (second store)))
                                (sheaf:root-summary-stores (summary summaries "PUT")))))))

(test the-existing-fixtures-pass-root-checking
  (dolist (fixture '("counter" "closures" "flows" "recursive-data" "stacks" "tail" "arrays" "cells" "placement" "vectors" "hashtables"))
    (is (hash-table-p (sheaf:check-roots (lowered fixture))) "~a" fixture)))

(test a-store-names-the-reference-and-the-store
  (flet ((chain (root) (chain-texts (root-error root))))
    (is (equal '("(arr:ref inner 0)" "(ref:set! slot (arr:ref inner 0))")
               (chain "REF-SET-INNER")))
    (is (equal '("(arr:ref inner 0)" "(arr:set! outer 0 (arr:ref inner 0))")
               (chain "ARR-SET-INNER")))
    (is (equal '("(arr:ref inner 0)" "(grow:push! outer (arr:ref inner 0))")
               (chain "PUSH-INNER")))
    (is (equal '("(arr:ref inner 0)" "(cell:write! outer (arr:ref inner 0))")
               (chain "CELL-WRITE-INNER")))
    (is (equal '("(arr:ref inner 0)" "(vector:push! (arr:ref inner 0) outer)")
               (chain "VECTOR-PUSH-INNER")))
    (is (equal '("(arr:ref inner 0)" "(hashtable:set! outer 1 (arr:ref inner 0))")
               (chain "HASHTABLE-SET-INNER")))
    (is (equal '("(cons 1 nil)" "(arr:set! outer 0 inner)")
               (chain "STORE-LIST-INNER")))
    (is (equal '("(arr:ref inner 0)" "(arr:set! outer 0 (arr:ref inner 0))")
               (chain "STORE-INTO-PARAM")))
    (is (equal '("(arr:ref inner 0)" "(arr:set! table 0 (arr:ref inner 0))")
               (chain "STORE-INTO-GLOBAL")))))

(test a-store-through-a-call-names-the-call-and-the-store-in-the-callee
  (is (equal '("(arr:ref inner 0)" "(put outer (arr:ref inner 0))" "(arr:set! a 0 r)")
             (chain-texts (root-error "STORE-THROUGH-CALL")))))

(test a-store-found-by-recursion-names-every-call
  (is (equal '("(arr:ref inner 0)" "(odd-put 3 outer (arr:ref inner 0))"
               "(even-put n a r)" "(arr:set! a 0 r)")
             (chain-texts (root-error "RECURSIVE-STORE")))))

(test a-reference-cannot-outlive-its-branch
  (let ((condition (root-error "ESCAPE-GROWN")))
    (is-true condition)
    (is (search "branch" (sheaf:specialization-error-message condition)))
    (destructuring-bind (reference match) (chain-texts condition)
      (is (equal "(arr:ref g 0)" reference))
      (is (eql 0 (search "(if " match))))))

(defun error-message (thunk)
  (handler-case (progn (funcall thunk) "")
    (error (condition) (princ-to-string condition))))

(test a-rejected-branch-escape-does-not-run-safely
  (dolist (root '("ESCAPE-GROWN" "ESCAPE-OWNED" "ESCAPE-CALLED"))
    (is-true (root-error root) "~a" root)
    (is (search "freed"
                (error-message
                 (lambda ()
                   (sheaf:run-lowered
                    (sheaf:assign-stacks (root-program "roots-rejected" root))))))
        "~a" root)))

(test a-returned-reference-keeps-its-array-in-the-destination
  (let ((clone (first (clones "roots" "FRESH-REF"))))
    (sheaf:walk-ir (lambda (node)
                     (when (and (typep node 'sheaf:ir-prim) (eq :array-new (sheaf:ir-prim-op node)))
                       (is (eql (sheaf:ir-def-destination clone) (sheaf:ir-prim-stack node)))))
                   (sheaf:ir-def-body clone))))
