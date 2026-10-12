;;;; loop-reuse.lisp
;;;; A for loop that builds cells in each iteration, which are given back when
;;;; the iteration ends unless a value stored outside the loop holds them.

(defpackage #:sheaf/fixtures/loop-reuse
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton/cell)))

(in-package #:sheaf/fixtures/loop-reuse)

(coalton-toplevel
  (declare len ((List UFix) -> UFix))
  (define (len xs)
    (match xs
      ((Nil) 0)
      ((Cons _ rest) (+ 1 (len rest)))))

  (declare churn (UFix -> UFix))
  (define (churn n)
    (let ((total (cell:new 0)))
      (for ((i 0 (+ i 1)))
        :while (< i n)
        (let ((xs (Cons i (Cons 1 Nil))))
          (cell:write! total (+ (cell:read total) (len xs)))
          (values)))
      (cell:read total)))

  (declare stored (UFix -> UFix))
  (define (stored n)
    (let ((box (cell:new (the (List UFix) Nil))))
      (for ((i 0 (+ i 1)))
        :while (< i n)
        (cell:write! box (Cons i Nil))
        (values))
      (len (cell:read box))))

  (declare main (Void -> UFix))
  (define (main)
    (+ (churn 100000) (stored 5))))
