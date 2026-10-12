;;;; loops.lisp
;;;; Loops that allocate on every iteration: one that carries nothing from an
;;;; iteration to the next, a pair of them, and one that passes a list through.

(defpackage #:sheaf/fixtures/loops
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/loops)

(coalton-toplevel
  (declare total (List UFix -> UFix))
  (define (total xs)
    (match xs
      ((Cons x rest) (+ x (total rest)))
      ((Nil) 0)))

  (declare scratch-loop (UFix * UFix -> UFix))
  (define (scratch-loop n acc)
    (if (== n 0)
        acc
        (scratch-loop (- n 1) (+ acc (total (Cons n (Cons 1 Nil)))))))

  (declare ping (UFix * UFix -> UFix))
  (define (ping n acc)
    (if (== n 0)
        acc
        (pong (- n 1) (+ acc (total (Cons n Nil))))))

  (declare pong (UFix * UFix -> UFix))
  (define (pong n acc)
    (if (== n 0)
        acc
        (ping (- n 1) (+ acc (total (Cons 2 (Cons n Nil)))))))

  (declare walk-list (UFix * List UFix * UFix -> UFix))
  (define (walk-list n xs acc)
    (if (== n 0)
        acc
        (walk-list (- n 1) xs (+ acc (total (Cons n xs))))))

  (declare run (UFix -> UFix))
  (define (run n)
    (+ (scratch-loop n 0)
       (+ (ping n 0)
          (walk-list n (Cons 5 Nil) 0))))

  (declare main (Void -> UFix))
  (define (main)
    (run 100))

  (declare big (Void -> UFix))
  (define (big)
    (run 1000000)))
