;;;; exceptions-stacks.lisp
;;;; Throws that unwind frames holding cells and banked vectors, in loops that would run out of stack if the unwinding leaked.

(defpackage #:sheaf/fixtures/exceptions-stacks
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/exceptions-stacks)

(coalton-toplevel
  (define-exception Over
    (Over UFix))

  (declare count-up (UFix * List UFix -> List UFix))
  (define (count-up n acc)
    (if (== n 0)
        acc
        (count-up (- n 1) (Cons n acc))))

  (declare total (List UFix -> UFix))
  (define (total xs)
    (match xs
      ((Cons x rest) (+ x (total rest)))
      ((Nil) 0)))

  (declare fail-with-list (UFix -> UFix))
  (define (fail-with-list n)
    (throw (Over (total (count-up n Nil)))))

  (declare attempts (UFix * UFix -> UFix))
  (define (attempts k acc)
    (if (== k 0)
        acc
        (catch (fail-with-list 100)
          ((Over x) (attempts (- k 1) (+ acc x))))))

  (declare vectors-down (UFix -> (List (vector:Vector UFix))))
  (define (vectors-down n)
    (let ((v (the (vector:Vector UFix) (vector:new))))
      (if (== n 0)
          Nil
          (Cons v (vectors-down (- n 1))))))

  (declare vector-count (List (vector:Vector UFix) -> UFix))
  (define (vector-count vectors)
    (match vectors
      ((Nil) 0)
      ((Cons v rest)
       (vector:push! 9 v)
       (+ (vector:length v) (vector-count rest)))))

  (declare fail-with-vectors (UFix -> UFix))
  (define (fail-with-vectors n)
    (throw (Over (vector-count (vectors-down n)))))

  (declare bank-attempts (UFix * UFix -> UFix))
  (define (bank-attempts k acc)
    (if (== k 0)
        acc
        (catch (fail-with-vectors 3)
          ((Over x) (bank-attempts (- k 1) (+ acc x))))))

  (declare countdown (UFix -> UFix))
  (define (countdown n)
    (if (== n 0)
        0
        (catch (countdown (- n 1))
          ((Over x) x))))

  (declare main-countdown (Void -> UFix))
  (define (main-countdown)
    (countdown 100))

  (declare main-lists-small (Void -> UFix))
  (define (main-lists-small)
    (attempts 20 0))

  (declare main-lists (Void -> UFix))
  (define (main-lists)
    (attempts 100000 0))

  (declare main-banks-small (Void -> UFix))
  (define (main-banks-small)
    (bank-attempts 5 0))

  (declare main-banks (Void -> UFix))
  (define (main-banks)
    (bank-attempts 1000 0))

  (declare main-unhandled (Void -> UFix))
  (define (main-unhandled)
    (+ 1 (fail-with-list 3)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (main-lists-small) (main-banks-small))))
