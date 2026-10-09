;;;; growable-recursion.lisp
;;;; A growable local kept across a call back into its recursive cycle.

(defpackage #:sheaf/fixtures/growable-recursion
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/growable-recursion)

(coalton-toplevel
  (declare kept (UFix -> UFix))
  (define (kept n)
    (let ((v (the (vector:Vector UFix) (vector:new))))
      (vector:push! n v)
      (if (== n 0)
          (vector:length v)
          (+ (kept (- n 1)) (vector:length v)))))

  (declare main (Void -> UFix))
  (define (main)
    (kept 3)))
