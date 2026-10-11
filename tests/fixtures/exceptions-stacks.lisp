;;;; exceptions-stacks.lisp
;;;; Throws that unwind frames holding cells and banked vectors, in loops that would run out of stack if the unwinding leaked, and
;;;; guarded bodies that build their result in a stack the handler's owner keeps.

(defpackage #:sheaf/fixtures/exceptions-stacks
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton/cell)
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

  (declare list-or-fail (UFix -> List UFix))
  (define (list-or-fail n)
    (let ((xs (count-up n Nil)))
      (if (== n 0) xs (throw (Over n)))))

  (declare retry-list (UFix -> List UFix))
  (define (retry-list k)
    (if (== k 0)
        (count-up 10 Nil)
        (catch (list-or-fail 100)
          ((Over _) (retry-list (- k 1))))))

  (declare retry-protected (UFix -> List UFix))
  (define (retry-protected k)
    (if (== k 0)
        (count-up 10 Nil)
        (catch (protect (list-or-fail 100) (total (count-up 3 Nil)))
          ((Over _) (retry-protected (- k 1))))))

  (declare vector-or-fail (UFix -> (vector:Vector UFix)))
  (define (vector-or-fail n)
    (let ((v (the (vector:Vector UFix) (vector:new))))
      (vector:push! n v)
      (if (== n 0) v (throw (Over n)))))

  (declare retry-vector (UFix -> (vector:Vector UFix)))
  (define (retry-vector k)
    (if (== k 0)
        (vector-or-fail 0)
        (catch (vector-or-fail 5)
          ((Over _) (retry-vector (- k 1))))))

  (declare banks-or-fail (UFix -> (List (vector:Vector UFix))))
  (define (banks-or-fail n)
    (let ((vectors (vectors-down n)))
      (if (== n 0) vectors (throw (Over n)))))

  (declare retry-banks (UFix -> (List (vector:Vector UFix))))
  (define (retry-banks k)
    (if (== k 0)
        (vectors-down 2)
        (catch (banks-or-fail 3)
          ((Over _) (retry-banks (- k 1))))))

  (declare stash (UFix -> UFix))
  (define (stash n)
    (let ((holder (the (cell:Cell (List UFix)) (cell:new Nil))))
      (catch (progn
               (cell:write! holder (count-up n Nil))
               (throw (Over n)))
        ((Over _) (total (cell:read holder))))))

  (declare main-retry-list-small (Void -> UFix))
  (define (main-retry-list-small)
    (total (retry-list 20)))

  (declare main-retry-list (Void -> UFix))
  (define (main-retry-list)
    (total (retry-list 100000)))

  (declare main-retry-protected-small (Void -> UFix))
  (define (main-retry-protected-small)
    (total (retry-protected 20)))

  (declare main-retry-protected (Void -> UFix))
  (define (main-retry-protected)
    (total (retry-protected 100000)))

  (declare main-retry-vector-small (Void -> UFix))
  (define (main-retry-vector-small)
    (vector:length (retry-vector 20)))

  (declare main-retry-vector (Void -> UFix))
  (define (main-retry-vector)
    (vector:length (retry-vector 100000)))

  (declare main-retry-banks-small (Void -> UFix))
  (define (main-retry-banks-small)
    (vector-count (retry-banks 5)))

  (declare main-retry-banks (Void -> UFix))
  (define (main-retry-banks)
    (vector-count (retry-banks 1000)))

  (declare main-stash (Void -> UFix))
  (define (main-stash)
    (stash 5))

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
