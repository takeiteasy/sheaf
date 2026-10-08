;;;; tail.lisp
;;;; A self tail call that runs ten million times, and a list built by one.

(defpackage #:sheaf/fixtures/tail
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/tail)

(coalton-toplevel
  (declare sum-to (UFix * UFix -> UFix))
  (define (sum-to n acc)
    (if (== n 0)
        acc
        (sum-to (- n 1) (+ acc 1))))

  (declare build (UFix * List UFix -> List UFix))
  (define (build n acc)
    (if (== n 0)
        acc
        (build (- n 1) (Cons n acc))))

  (declare total (List UFix -> UFix))
  (define (total xs)
    (match xs
      ((Cons x rest) (+ x (total rest)))
      ((Nil) 0)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (sum-to 10000000 0)
       (total (build 1000 Nil)))))
