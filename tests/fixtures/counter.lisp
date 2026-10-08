;;;; counter.lisp
;;;; An iterator chain and a closure returned over a Cell.

(defpackage #:sheaf/fixtures/counter
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:iter #:coalton/iterator)
   (#:cell #:coalton/cell)))

(in-package #:sheaf/fixtures/counter)

(coalton-toplevel
  (declare sum-squares (UFix -> UFix))
  (define (sum-squares n)
    (iter:fold! + 0 (map (fn (x) (* x x)) (iter:up-to n))))

  (declare make-counter (Void -> (Void -> UFix)))
  (define (make-counter)
    (let ((c (cell:new 0)))
      (fn () (cell:update! (fn (x) (+ x 1)) c))))

  (declare main (Void -> UFix))
  (define (main)
    (let ((next (make-counter)))
      (next)
      (+ (sum-squares 10) (next)))))
