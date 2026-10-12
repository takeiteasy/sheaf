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

(defun handler-sets (assigned type)
  "For each handler of TYPE in ASSIGNED, its saved stacks, saved banks and busy stacks."
  (let ((sets '()))
    (maphash (lambda (name def)
               (declare (ignore name))
               (sheaf:walk-ir (lambda (node)
                                (when (typep node type)
                                  (push (etypecase node
                                          (sheaf:ir-catch (list (sheaf:ir-catch-marks node) (sheaf:ir-catch-bank-marks node)
                                                                (sheaf:ir-catch-busy node)))
                                          (sheaf:ir-protect (list (sheaf:ir-protect-marks node) (sheaf:ir-protect-bank-marks node)
                                                                  (sheaf:ir-protect-busy node))))
                                        sets)))
                              (sheaf:ir-def-body def)))
             (sheaf:lowered-defs assigned))
    sets))

(defun gives-back-busy-p (assigned type)
  "Whether some handler of TYPE in ASSIGNED saves a stack or bank that is in use where it is entered."
  (some (lambda (set)
          (destructuring-bind (marks bank-marks busy) set
            (or (intersection marks busy) (intersection bank-marks busy))))
        (handler-sets assigned type)))

(test a-handler-saves-only-what-its-body-builds-on
  (let ((sets (handler-sets (stacks-from "MAIN-COUNTDOWN") 'sheaf:ir-catch)))
    (is (plusp (length sets)))
    (is (every (lambda (set) (and (null (first set)) (null (second set)))) sets)))
  (let ((assigned (stacks-from "MAIN-BANKS")))
    (is (every (lambda (set) (subsetp (first set) (loop :for i :below (sheaf:lowered-stack-count assigned) :collect i)))
               (handler-sets assigned 'sheaf:ir-catch)))
    (is (some (lambda (set) (second set)) (handler-sets assigned 'sheaf:ir-catch)))))

(test a-catch-gives-back-cells-its-body-built-in-a-stack-in-use
  (dolist (root '("MAIN-RETRY-LIST-SMALL" "MAIN-RETRY-PROTECTED-SMALL" "MAIN-RETRY-VECTOR-SMALL" "MAIN-RETRY-BANKS-SMALL"))
    (let ((assigned (stacks-from root)))
      (is-true (gives-back-busy-p assigned 'sheaf:ir-catch) "~a" root)
      (is (= (funcall (symbol-function (fixture-symbol "exceptions-stacks" root)))
             (sheaf:run-lowered assigned)
             (parse-integer (run-c assigned)))
          "~a" root)))
  (is-true (gives-back-busy-p (stacks-from "MAIN-RETRY-PROTECTED-SMALL") 'sheaf:ir-protect)))

(test a-retried-catch-does-not-grow-the-stack-in-use
  (loop :for (root expected) :in '(("MAIN-RETRY-LIST" 55) ("MAIN-RETRY-PROTECTED" 55) ("MAIN-RETRY-VECTOR" 1))
        :do (multiple-value-bind (output errors code)
                (run-c (stacks-from root) "-DSHEAF_STACK_SIZE=1048576")
              (declare (ignore errors))
              (is (zerop code) "~a" root)
              (is (= expected (parse-integer output)) "~a" root)))
  (multiple-value-bind (output errors code) (apply #'run-c (stacks-from "MAIN-RETRY-BANKS") *small-banks*)
    (declare (ignore errors))
    (is (zerop code))
    (is (= 2 (parse-integer output)))))

(test a-catch-keeps-cells-its-body-stores-outside-itself
  (let ((assigned (stacks-from "MAIN-STASH")))
    (is-false (gives-back-busy-p assigned 'sheaf:ir-catch))
    (is (= 15 (funcall (symbol-function (fixture-symbol "exceptions-stacks" "MAIN-STASH")))))
    (is (= 15 (sheaf:run-lowered assigned)))
    (is (= 15 (parse-integer (run-c assigned))))))

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

(test panics-give-one-result-on-every-tier
  (let ((expected (run-on-sbcl "panics")))
    (is (= 2038 expected))
    (is (zerop (count-ir (lowered "panics") 'sheaf:ir-foreign)))
    (is (= expected (sheaf:run-lowered (lowered "panics"))))
    (is (= expected (sheaf:run-lowered (assigned "panics"))))
    (is (= expected (c-result "panics")))))

(test an-unhandled-panic-ends-the-program
  (signals sheaf:panic (sheaf:run-lowered (lowered "panics-uncaught")))
  (multiple-value-bind (output errors code) (run-c (assigned "panics-uncaught"))
    (declare (ignore output))
    (is (/= 0 code))
    (is (search "Assertion" errors))))

(test raw-lisp-with-no-translation-is-an-error-at-its-location
  (dolist (root '("MAIN-FORMAT" "MAIN-RAW"))
    (let ((message (error-message (lambda () (sheaf:emit-c (assigned (list "panics-rejected" root)))))))
      (is (search "raw Lisp" message) "~a" root)
      (is (search "at offset" message) "~a" root))))
