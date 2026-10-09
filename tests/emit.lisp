;;;; emit.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defun build-c (assigned binary &rest options)
  "Compile ASSIGNED's C file to BINARY, signalling an error with the compiler's
output if it fails."
  (uiop:with-temporary-file (:pathname source :type "c")
    (sheaf:write-c assigned source)
    (multiple-value-bind (output errors code)
        (uiop:run-program
         (append (list (or (uiop:getenv "CC") "cc") "-std=c11" "-Wall" "-Wextra" "-Werror"
                       "-I" (namestring (asdf:system-relative-pathname "sheaf" "runtime/")))
                 options
                 (list "-o" (namestring binary) (namestring source)))
         :output :string :error-output :string :ignore-error-status t)
      (declare (ignore output))
      (unless (zerop code)
        (error "The C compiler failed:~%~a" errors)))))

(defun run-c (assigned &rest options)
  "Compile and run ASSIGNED's C file. Returns its standard output, standard error
and exit code."
  (uiop:with-temporary-file (:pathname binary :type "bin")
    (apply #'build-c assigned binary options)
    (uiop:run-program (list (namestring binary))
                      :output :string :error-output :string :ignore-error-status t)))

(defun c-result (fixture)
  (parse-integer (run-c (assigned fixture))))

(test compiled-programs-match-the-interpreter
  (is (= 51 (c-result "closures")))
  (is (= 55 (c-result "flows")))
  (is (= 21 (c-result "recursive-data")))
  (is (= 47 (c-result "stacks"))))

(test a-self-tail-call-is-a-loop
  (is (= 10500500 (c-result "tail"))))

(test an-exhausted-data-stack-aborts
  (multiple-value-bind (output errors code) (run-c (assigned "tail") "-DSHEAF_STACK_SIZE=4096")
    (declare (ignore output))
    (is (/= 0 code))
    (is (search "exhausted" errors))))

(test raw-lisp-is-not-emitted
  (signals error (sheaf:emit-c (assigned "counter"))))

(defun c-result-from (fixture root)
  "The result of the C program for the function ROOT of FIXTURE, and the interpreter's."
  (let ((assigned (sheaf:assign-stacks (lowered-from fixture root))))
    (values (parse-integer (run-c assigned)) (sheaf:run-lowered assigned))))

(test prelude-programs-match-the-interpreter-in-c
  (is (= 116 (c-result "arrays")))
  (is (= 29 (c-result "cells")))
  (is (= 53 (c-result "vectors"))))

(test growables-match-the-interpreter-in-c
  (dolist (root '("MAIN-TWO" "MAIN-LIST" "MAIN-RELAY" "MAIN-EITHER" "MAIN-LISTED" "MAIN-DOWN"
                  "MAIN-PICKED" "MAIN-CELL" "MAIN-TABLE" "MAIN-VECTORS" "MAIN-TWOS" "MAIN-DEEP"
                  "MAIN-REUSE"))
    (multiple-value-bind (compiled interpreted) (c-result-from "growables" root)
      (is (= interpreted compiled) "~a" root))))

(test an-index-outside-an-array-aborts-in-c
  (multiple-value-bind (output errors code) (run-c (assigned "out-of-bounds"))
    (declare (ignore output))
    (is (/= 0 code))
    (is (search "outside" errors))))

(test a-growable-under-another-value-aborts-in-c
  (multiple-value-bind (output errors code) (run-c (with-shared-stack "arrays" "TWO-ARRAYS"))
    (declare (ignore output))
    (is (/= 0 code))
    (is (search "grew under" errors))))

(test hash-tables-are-not-emitted
  (is (search "sheaf/32" (error-text (lambda () (sheaf:emit-c (assigned "hashtables")))))))

(defparameter *small-banks* '("-DSHEAF_SLICE_SIZE=65536" "-DSHEAF_BANK_SPACE=524288")
  "Eight slices of bank space.")

(defun banked-c (root &rest options)
  (apply #'run-c (sheaf:assign-stacks (lowered-from "growables" root)) options))

(test a-bank-hands-out-slices-and-takes-them-back
  (multiple-value-bind (output errors code) (apply #'banked-c "MAIN-REUSE" *small-banks*)
    (declare (ignore errors))
    (is (zerop code))
    (is (= (sheaf:run-lowered (sheaf:assign-stacks (lowered-from "growables" "MAIN-REUSE")))
           (parse-integer output)))))

(test a-cycle-deeper-than-the-bank-space-aborts-in-c
  (multiple-value-bind (output errors code) (apply #'banked-c "MAIN-DEEP" *small-banks*)
    (declare (ignore output))
    (is (/= 0 code))
    (is (search "bank space exhausted" errors))))

(test clones-are-named-in-comments
  (is (search "// PAIR<" (sheaf:emit-c (assigned "stacks")))))
