;;;; stdlib.lisp
;;;; Standard library functions written in Coalton, run on Sheaf's data.

(defpackage #:sheaf/fixtures/stdlib
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton/cell)))

(in-package #:sheaf/fixtures/stdlib)

(coalton-toplevel
  (declare updated (UFix -> UFix))
  (define (updated n)
    (let ((c (cell:new n)))
      (cell:update! (fn (x) (+ x 1)) c)))

  (declare update-swapped (UFix -> UFix))
  (define (update-swapped n)
    (let ((c (cell:new n)))
      (+ (cell:update-swap! (fn (x) (* x 2)) c) (cell:read c))))

  (declare main (Void -> UFix))
  (define (main)
    (+ (updated 3) (update-swapped 5))))
