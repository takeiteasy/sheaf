;;;; stacks.lisp
;;;; Lists built and returned, built and dropped, built inside a recursive
;;;; cycle, and bound to a global.

(defpackage #:sheaf/fixtures/stacks
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/stacks)

(coalton-toplevel
  (declare pair (UFix * UFix -> List UFix))
  (define (pair a b)
    (Cons a (Cons b Nil)))

  (declare total (List UFix -> UFix))
  (define (total xs)
    (match xs
      ((Cons x rest) (+ x (total rest)))
      ((Nil) 0)))

  (declare pass (List UFix -> List UFix))
  (define (pass xs)
    xs)

  (declare through-helper (UFix -> List UFix))
  (define (through-helper a)
    (pass (pair a a)))

  (declare dropped (UFix -> UFix))
  (define (dropped a)
    (total (pair a (+ a 1))))

  (declare twice (UFix -> UFix))
  (define (twice a)
    (+ (dropped a) (dropped (+ a 1))))

  (declare count-down (UFix -> List UFix))
  (define (count-down n)
    (if (== n 0)
        Nil
        (Cons n (count-down (- n 1)))))

  (declare walk (UFix -> UFix))
  (define (walk n)
    (if (== n 0)
        0
        (+ (total (pair n n)) (walk (- n 1)))))

  (declare table (List UFix))
  (define table
    (Cons 5 (Cons 6 Nil)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (total (through-helper 1))
       (+ (twice 2)
          (+ (total (count-down 4))
             (+ (walk 3)
                (total table)))))))
