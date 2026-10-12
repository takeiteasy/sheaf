;;;; vector-folds.lisp
;;;; Folds, sorts and a search over a Vector, with functions that capture a variable.

(defpackage #:sheaf/fixtures/vector-folds
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/vector-folds)

(coalton-toplevel
  (declare scrambled (Void -> (Vector UFix)))
  (define (scrambled)
    (let ((v (the (Vector UFix) (vector:new))))
      (vector:push! 5 v)
      (vector:push! 3 v)
      (vector:push! 1 v)
      (vector:push! 4 v)
      (vector:push! 2 v)
      v))

  (declare digits ((Vector UFix) -> UFix))
  (define (digits v)
    (fold (fn (acc x) (+ (* acc 10) x)) 0 v))

  (declare folded (Void -> UFix))
  (define (folded)
    (let ((v (scrambled))
          (k 7))
      (+ (+ (fold + 0 v) (fold (fn (acc x) (+ acc (* x k))) 0 v))
         (+ (digits v) (foldr (fn (x acc) (+ (* acc 10) x)) 0 v)))))

  (declare sorted (Void -> UFix))
  (define (sorted)
    (let ((v (scrambled))
          (w (scrambled))
          (e (the (Vector UFix) (vector:new)))
          (offset 1))
      (vector:sort! v)
      (vector:sort-by! (fn (a b) (> (+ a offset) (+ b offset))) w)
      (vector:sort! e)
      (+ (+ (digits v) (* 1000000 (digits w))) (vector:length e))))

  (declare found (Void -> UFix))
  (define (found)
    (let ((v (scrambled)))
      (+ (match (vector:find-elem 3 v)
           ((Some i) (+ i 100))
           ((None) 0))
         (match (vector:find-elem 9 v)
           ((Some i) (+ i 1000))
           ((None) 7)))))

  (declare main (Void -> UFix))
  (define (main)
    (+ (folded) (+ (sorted) (found)))))
