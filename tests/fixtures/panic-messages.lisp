;;;; panic-messages.lisp
;;;; Panics whose messages are built at run time, caught and not.

(defpackage #:sheaf/fixtures/panic-messages
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:string #:coalton/string)))

(in-package #:sheaf/fixtures/panic-messages)

(coalton-toplevel
  (define-type Shape
    (Disc UFix)
    (Square UFix UFix)
    Blank)

  (declare error-built (Void -> UFix))
  (define (error-built)
    (error (string:concat "bad " (string:concat "thing " "é"))))

  (declare error-caught (Void -> UFix))
  (define (error-caught)
    (catch (error (string:concat "bad " "thing"))
      (_ 7)))

  (declare assert-format (Void -> UFix))
  (define (assert-format)
    (assert (> (the UFix 3) 5) "n is ~a and ~d, ~s" (the UFix 3) (the I8 -4) "q")
    1)

  (declare assert-floats (Void -> UFix))
  (define (assert-floats)
    (assert False "~a ~a ~a ~a" (the F32 1234.5) (the F32 0.001) (the F64 (fromInt 100)) (the F64 (fromInt 12345678)))
    1)

  (declare assert-shapes (Void -> UFix))
  (define (assert-shapes)
    (assert False "~a ~a ~a ~a ~a" (Square 2 3) Blank True #\x (Some (Disc 1)))
    1)

  (declare assert-text (Void -> UFix))
  (define (assert-text)
    (assert False "100%~% done ~~")
    1)

  (declare assert-caught (UFix -> UFix))
  (define (assert-caught n)
    (catch (progn (assert (> n 5) "n is ~a" n) n)
      (_ 9)))

  (declare unwrap-none (Void -> UFix))
  (define (unwrap-none)
    (unwrap (the (Optional UFix) None)))

  (declare unwrap-err (Void -> UFix))
  (define (unwrap-err)
    (unwrap (the (Result Shape UFix) (Err (Square 4 5)))))

  (declare unwrap-ok (Void -> UFix))
  (define (unwrap-ok)
    (unwrap (the (Optional UFix) (Some 4))))

  (declare expect-none (Void -> UFix))
  (define (expect-none)
    (expect "no value here" (the (Optional UFix) None)))

  (declare expect-caught (Void -> UFix))
  (define (expect-caught)
    (catch (expect "no value here" (the (Optional UFix) None))
      (_ 11)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (error-caught)
       (+ (assert-caught 3)
          (+ (assert-caught 8)
             (+ (unwrap-ok)
                (expect-caught)))))))
