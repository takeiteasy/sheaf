;;;; numbers.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defparameter *number-roots*
  '(("MAIN-U8" 4) ("MAIN-U16" 1) ("MAIN-U32" 4294967295) ("MAIN-U64" 18446744073709551615)
    ("MAIN-I8" -5) ("MAIN-I16" 32761) ("MAIN-I32" 2147395600) ("MAIN-I64" -9223372036854775808)
    ("MAIN-IFIX" 3) ("MAIN-INTEGER" 9000000000)
    ("MAIN-F64" 0.75d0) ("MAIN-F32" 4.5f0)
    ("MAIN-ARRAY" 3.0d0) ("MAIN-TABLE" 200) ("MAIN-NARROW" 44) ("MAIN-SQUEEZE" 100) ("MAIN-COMPARE" 15))
  "The roots of the numbers fixture that run, and what each gives.")

(defun read-number (text)
  (let ((*read-eval* nil)
        (*read-default-float-format* 'double-float))
    (read-from-string text)))

(test numbers-give-one-result-on-every-tier
  (loop :for (root expected) :in *number-roots*
        :do (let ((lowered (lowered-from "numbers" root)))
              (is (= expected (funcall (symbol-function (fixture-symbol "numbers" root)))) "~a on SBCL" root)
              (is (= expected (sheaf:run-lowered lowered)) "~a interpreted" root)
              (is (= expected (read-number (run-c (sheaf:assign-stacks lowered)))) "~a in C" root))))

(test numeric-methods-lower-to-primitives
  (let ((ops '()))
    (loop :for (root) :in *number-roots*
          :do (let ((lowered (lowered-from "numbers" root)))
                (is-true (sheaf:check-lowered lowered) "~a" root)
                (is (zerop (count-ir lowered 'sheaf:ir-foreign)) "~a" root)
                (dolist (node (prims lowered))
                  (unless (sheaf::prelude-op-p (sheaf:ir-prim-op node))
                    (pushnew (sheaf:ir-prim-op node) ops)))))
    (is (equal '(:add :compare :eq :from-int :mul :sub) (sort ops #'string<)))))

(test a-trapping-type-overflows-in-the-interpreter-and-in-c
  (dolist (root '("MAIN-OVERFLOW" "MAIN-TOO-WIDE"))
    (let ((lowered (lowered-from "numbers" root)))
      (is (search "Overflow" (error-text (lambda () (sheaf:run-lowered lowered)))) "~a" root)
      (multiple-value-bind (output errors code) (run-c (sheaf:assign-stacks lowered))
        (declare (ignore output))
        (is (/= 0 code) "~a" root)
        (is (search "overflow" errors) "~a" root)))))

(test a-literal-that-does-not-fit-is-a-lowering-error
  (is (search "does not fit"
              (error-text (lambda () (lowered-from "numbers" "MAIN-BIG-LITERAL"))))))

(test wrapping-types-reduce-to-their-width
  (is (= 255 (sheaf::fit 'coalton:u8 -1)))
  (is (= 0 (sheaf::fit 'coalton:ufix (expt 2 64)))))

(test float-types-convert-integers
  (is (eql 3.0f0 (sheaf::fit 'coalton:f32 3)))
  (is (eql 3.0d0 (sheaf::fit 'coalton:f64 3)))
  (is (eql float-features:single-float-positive-infinity (sheaf::fit 'coalton:f32 (expt 10 400)))))
