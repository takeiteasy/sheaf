;;;; loop-exits.lisp
;;;; A Vector built in each iteration and given back when the iteration ends
;;;; by continuing, by breaking, and by returning from the function.

(defpackage #:sheaf/fixtures/loop-exits
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton/cell)
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/loop-exits)

(coalton-toplevel
  (declare churn (UFix -> UFix))
  (define (churn n)
    (let ((total (cell:new 0)))
      (for ((i 0 (+ i 1)))
        :while (< i n)
        (let ((v (the (Vector UFix) (vector:new))))
          (vector:push! i v)
          (vector:push! 1 v)
          (when (< i 90000)
            (continue))
          (cell:write! total (+ (cell:read total) (vector:length v)))
          (when (> i 99990)
            (break))
          (values)))
      (cell:read total)))

  (declare escaping (UFix -> UFix))
  (define (escaping n)
    (for ((i 0 (+ i 1)))
      :while (< i n)
      (let ((v (the (Vector UFix) (vector:new))))
        (vector:push! i v)
        (vector:push! 1 v)
        (when (> i 50000)
          (return (vector:length v)))
        (values)))
    0)

  (declare main (Void -> UFix))
  (define (main)
    (+ (churn 100000) (escaping 100000))))
