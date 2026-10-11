;;;; exceptions.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defun stacks-from (root)
  "The function ROOT of exceptions-stacks, with its stacks assigned."
  (sheaf:assign-stacks (lowered-from "exceptions-stacks" root)))

(test exceptions-give-one-result-on-every-tier
  (let ((expected (run-on-sbcl "exceptions")))
    (is (= 2689 expected))
    (is (= expected (run-specialized (defunctionalized "exceptions"))))
    (is (= expected (sheaf:run-lowered (lowered "exceptions"))))
    (is (= expected (sheaf:run-lowered (assigned "exceptions"))))
    (is (= expected (c-result "exceptions")))))

(test a-cleanup-keeps-the-exception-it-runs-for
  (loop :for (root expected) :in '(("MAIN-CLEANUP-CATCHES" 49) ("MAIN-CLEANUP-THROWS" 154)
                                   ("MAIN-NESTED-CLEANUPS" 150))
        :do (let ((assigned (sheaf:assign-stacks (lowered-from "exceptions" root))))
              (is (= expected (funcall (symbol-function (fixture-symbol "exceptions" root)))) "~a on SBCL" root)
              (is (= expected (sheaf:run-lowered assigned)) "~a in the interpreter" root)
              (is (= expected (parse-integer (run-c assigned))) "~a in C" root))))

(test exceptions-lower-to-nodes
  (let ((lowered (lowered "exceptions")))
    (is-true (sheaf:check-lowered lowered))
    (is (zerop (count-ir lowered 'sheaf:ir-foreign)))
    (is (= 11 (count-ir lowered 'sheaf:ir-catch)))
    (is (= 5 (count-ir lowered 'sheaf:ir-protect)))
    (is (= 7 (count-ir lowered 'sheaf:ir-throw)))))

(test a-handler-records-the-stacks-in-use
  (let ((assigned (assigned "exceptions-stacks")) (seen 0))
    (maphash (lambda (name def)
               (declare (ignore name))
               (sheaf:walk-ir (lambda (node)
                                (when (typep node 'sheaf:ir-catch)
                                  (incf seen)
                                  (is (every (lambda (index) (< -1 index (sheaf:lowered-stack-count assigned)))
                                             (sheaf:ir-catch-busy node)))))
                              (sheaf:ir-def-body def)))
             (sheaf:lowered-defs assigned))
    (is (plusp seen))))

(test exceptions-match-the-interpreter-in-c
  (dolist (root '("MAIN-LISTS-SMALL" "MAIN-BANKS-SMALL" "MAIN-COUNTDOWN"))
    (let ((assigned (stacks-from root)))
      (is (= (sheaf:run-lowered assigned) (parse-integer (run-c assigned))) "~a" root))))

(test a-throw-gives-its-stacks-back-to-the-catch
  (multiple-value-bind (output errors code)
      (run-c (stacks-from "MAIN-LISTS") "-DSHEAF_STACK_SIZE=1048576")
    (declare (ignore errors))
    (is (zerop code))
    (is (= 505000000 (parse-integer output)))))

(test a-throw-gives-its-bank-slices-back-to-the-catch
  (multiple-value-bind (output errors code) (apply #'run-c (stacks-from "MAIN-BANKS") *small-banks*)
    (declare (ignore errors))
    (is (zerop code))
    (is (= 3000 (parse-integer output)))))

(test a-tail-call-in-a-catch-branch-is-a-jump
  (is (nodes-of (sheaf:c-program (stacks-from "MAIN-LISTS-SMALL")) :goto)))

(defun function-node (program name)
  "The `:fn` node in PROGRAM whose comment starts with NAME."
  (loop :for (comment node) :on program
        :when (and (eq (first comment) :comment) (eql 0 (search name (second comment))))
          :return node))

(test a-call-in-a-guarded-body-is-not-a-jump
  (let ((program (sheaf:c-program (stacks-from "MAIN-COUNTDOWN"))))
    (is-true (function-node program "COUNTDOWN"))
    (is (null (nodes-of (function-node program "COUNTDOWN") :goto)))))

(test an-unhandled-exception-ends-the-program
  (multiple-value-bind (output errors code) (run-c (stacks-from "MAIN-UNHANDLED"))
    (declare (ignore output))
    (is (= 1 code))
    (is (search "unhandled exception OVER" errors)))
  (signals error (sheaf:run-lowered (stacks-from "MAIN-UNHANDLED"))))

(test an-exception-that-holds-storage-or-links-cells-is-an-error
  (let ((vector (error-message (lambda () (lowered-from "exceptions-rejected" "THROW-VECTOR"))))
        (list (error-message (lambda () (lowered-from "exceptions-rejected" "THROW-LIST")))))
    (is (search "exception type HEAVY, which holds VECTOR UFIX" vector))
    (is (search "exception type LINKED, which holds LIST UFIX, which contains itself" list))))

(test a-handler-that-runs-before-unwinding-is-an-error
  (dolist (root '("MAIN-HANDLE" "MAIN-RESUMABLE"))
    (let ((message (error-message
                    (lambda ()
                      (sheaf:defunctionalize
                       (sheaf:monomorphize (fixture-program "exceptions-rejected")
                                           (fixture-symbol "exceptions-rejected" root)))))))
      (is (search "issues/24" message) "~a" root))))
