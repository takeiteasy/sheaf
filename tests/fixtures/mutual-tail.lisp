;;;; mutual-tail.lisp
;;;; Tail calls between functions: a pair, and a cycle of three with different parameters.

(defpackage #:sheaf/fixtures/mutual-tail
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/mutual-tail)

(coalton-toplevel
  (declare even-p (UFix -> UFix))
  (define (even-p n)
    (if (== n 0)
        1
        (odd-p (- n 1))))

  (declare odd-p (UFix -> UFix))
  (define (odd-p n)
    (if (== n 0)
        0
        (even-p (- n 1))))

  (declare step-a (UFix * UFix -> UFix))
  (define (step-a n acc)
    (if (== n 0)
        acc
        (step-b (- n 1) (+ acc 1) 2)))

  (declare step-b (UFix * UFix * I32 -> UFix))
  (define (step-b n acc by)
    (if (== n 0)
        acc
        (if (== by 2)
            (step-c (- n 1) (+ acc 2))
            (step-c (- n 1) acc))))

  (declare step-c (UFix * UFix -> UFix))
  (define (step-c n acc)
    (if (== n 0)
        acc
        (step-a (- n 1) (+ acc 3))))

  (declare main (Void -> UFix))
  (define (main)
    (+ (even-p 10000000)
       (step-a 3000000 0))))
