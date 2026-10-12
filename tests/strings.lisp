;;;; strings.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defparameter *string-roots*
  '(("MAIN-CHAR-CODE" 65) ("MAIN-CHAR-ORDER" 3) ("MAIN-CODE-CHAR" 299) ("MAIN-ASCII" 231)
    ("MAIN-LENGTH" 85) ("MAIN-EMPTY" 0) ("MAIN-CONCAT" 6) ("MAIN-SEMIGROUP" 4)
    ("MAIN-SUBSTRING" 225) ("MAIN-REF" 355) ("MAIN-REF-NONE" 2) ("MAIN-EQUAL" 5)
    ("MAIN-ORDER" 21) ("MAIN-PREFIX" 23) ("MAIN-CHARS" 428) ("MAIN-INTEGERS" 135)
    ("MAIN-BUILT" 2010) ("MAIN-LOOP" 5000) ("MAIN" 3868))
  "The roots of the strings fixture that run on SBCL, and what each gives.")

(test chars-and-strings-give-one-result-on-every-tier
  (loop :for (root expected) :in *string-roots*
        :do (let ((lowered (lowered-from "strings" root)))
              (is (= expected (funcall (symbol-function (fixture-symbol "strings" root)))) "~a on SBCL" root)
              (is (zerop (count-ir lowered 'sheaf:ir-foreign)) "~a" root)
              (is (= expected (sheaf:run-lowered lowered)) "~a interpreted" root)
              (is (= expected (sheaf:run-lowered (sheaf:assign-stacks lowered))) "~a on stacks" root)
              (is (= expected (parse-integer (run-c (sheaf:assign-stacks lowered)))) "~a in C" root))))

(test a-code-past-the-last-character-is-none-on-every-tier-but-sbcl
  (let ((lowered (lowered-from "strings" "MAIN-CODE-CHAR-NONE")))
    (is (= 2 (sheaf:run-lowered lowered)))
    (is (= 2 (parse-integer (run-c (sheaf:assign-stacks lowered)))))))

(test an-index-or-range-outside-a-string-is-an-error-on-every-tier
  (loop :for (root text) :in '(("MAIN-INDEX-ERROR" "index 5 is outside the string of 3 characters")
                               ("MAIN-RANGE-ERROR" "range 10 to 5 is outside the string of 5 characters"))
        :do (let ((lowered (lowered-from "strings" root)))
              (is (search text (error-text (lambda () (sheaf:run-lowered lowered)))) "~a" root)
              (multiple-value-bind (output errors code) (run-c (sheaf:assign-stacks lowered))
                (declare (ignore output))
                (is (/= 0 code) "~a" root)
                (is (search text errors) "~a" root)))))
