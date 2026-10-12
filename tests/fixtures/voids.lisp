;;;; voids.lisp
;;;; A function returning Void whose branches produce no values.

(defpackage #:sheaf/fixtures/voids
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton/cell)))

(in-package #:sheaf/fixtures/voids)

(coalton-toplevel
  (declare record! ((Cell UFix) * UFix -> Void))
  (define (record! c x)
    (when (> x 2)
      (cell:write! c x)))

  (declare main (Void -> UFix))
  (define (main)
    (let ((c (cell:new 0)))
      (record! c 1)
      (record! c 5)
      (record! c 2)
      (cell:read c))))
