;;;; for-loops.lisp
;;;; for loops: each kind of termination, parallel and sequential steps, break,
;;;; continue, a result, a list carried in a variable, a Vector built in the
;;;; body, nested loops and a return out of a loop.

(defpackage #:sheaf/fixtures/for-loops
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton/cell)
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/for-loops)

(coalton-toplevel
  (declare len ((List UFix) -> UFix))
  (define (len xs)
    (match xs
      ((Nil) 0)
      ((Cons _ rest) (+ 1 (len rest)))))

  (declare sum-while (UFix -> UFix))
  (define (sum-while n)
    (for ((i 0 (+ i 1))
          (acc 0 (+ acc i)))
      :returns acc
      :while (< i n)
      (values)))

  (declare sum-until (UFix -> UFix))
  (define (sum-until n)
    (for ((i 0 (+ i 1))
          (acc 0 (+ acc i)))
      :returns acc
      :until (> i n)
      (values)))

  (declare doubled (UFix -> UFix))
  (define (doubled n)
    (for ((acc 1 (* acc 2)))
      :returns acc
      :repeat n
      (values)))

  (declare sequential (UFix -> UFix))
  (define (sequential n)
    (for* ((a 1 (+ a 1))
           (b (+ a 10) (+ a b)))
      :returns (+ (* a 100) b)
      :repeat n
      (values)))

  (declare skipping (UFix -> UFix))
  (define (skipping n)
    (let ((total (cell:new 0)))
      (for ((i 0 (+ i 1)))
        :returns (cell:read total)
        :while (< i n)
        (when (== i 3)
          (continue))
        (when (> i 6)
          (break))
        (cell:write! total (+ (cell:read total) i))
        (values))))

  (declare carried (UFix -> UFix))
  (define (carried n)
    (for ((i 0 (+ i 1))
          (xs (the (List UFix) Nil) (Cons i xs)))
      :returns (len xs)
      :while (< i n)
      (values)))

  (declare building (UFix -> UFix))
  (define (building n)
    (let ((total (cell:new 0)))
      (for ((i 0 (+ i 1)))
        :returns (cell:read total)
        :while (< i n)
        (let ((v (the (Vector UFix) (vector:new))))
          (vector:push! i v)
          (vector:push! 7 v)
          (cell:write! total (+ (cell:read total) (vector:length v)))
          (values)))))

  (declare nested (UFix -> UFix))
  (define (nested n)
    (let ((total (cell:new 0)))
      (for ((i 0 (+ i 1)))
        :returns (cell:read total)
        :while (< i n)
        (for ((j 0 (+ j 1)))
          :while (< j n)
          (when (> j i)
            (break))
          (cell:write! total (+ (cell:read total) 1))
          (values)))))

  (declare leaving (UFix -> UFix))
  (define (leaving n)
    (for ((i 0 (+ i 1)))
      :while (< i 100)
      (when (> (* i i) n)
        (return i))
      (values))
    0)

  (declare main (Void -> UFix))
  (define (main)
    (+ (+ (sum-while 5) (sum-until 4))
       (+ (+ (doubled 4) (sequential 3))
          (+ (+ (skipping 10) (carried 4))
             (+ (+ (building 3) (nested 4))
                (leaving 30)))))))
