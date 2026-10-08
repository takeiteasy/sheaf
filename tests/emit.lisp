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

(test references-are-not-emitted
  (signals error
    (sheaf:emit-c (hand-built *number*
                              (lambda (make-call)
                                (sheaf::make-ir-let :name 'v :value (funcall make-call)
                                                    :type *number* :body (zero)))))))

(test clones-are-named-in-comments
  (is (search "// PAIR<" (sheaf:emit-c (assigned "stacks")))))
