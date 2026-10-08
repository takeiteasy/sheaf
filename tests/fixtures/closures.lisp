;;;; closures.lisp
;;;; Lambdas reaching one slot, a function in a constructor, a returned
;;;; closure, a recursive local function and a global used as a value.

(defpackage #:sheaf/fixtures/closures
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/closures)

(coalton-toplevel
  (define-type Handler
    (Handler (UFix -> UFix)))

  (declare apply-twice ((UFix -> UFix) * UFix -> UFix))
  (define (apply-twice f x)
    (f (f x)))

  (declare double (UFix -> UFix))
  (define (double x)
    (* x 2))

  (declare adder (UFix -> (UFix -> UFix)))
  (define (adder n)
    (fn (x) (+ x n)))

  (declare pick (UFix -> (UFix -> UFix)))
  (define (pick n)
    (if (> n 5)
        (adder n)
        double))

  (declare run-handler (Handler * UFix -> UFix))
  (define (run-handler h x)
    (match h
      ((Handler f) (f x))))

  (declare sum-to (UFix -> UFix))
  (define (sum-to n)
    (let ((go (fn (k acc)
                (if (== k 0)
                    acc
                    (go (- k 1) (+ acc k))))))
      (go n 0)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (apply-twice (adder 3) 1)
       (+ (apply-twice (fn (x) (* x 3)) 1)
          (+ ((pick 10) 1)
             (+ ((pick 2) 4)
                (+ (run-handler (Handler (adder 1)) 5)
                   (sum-to 4))))))))
