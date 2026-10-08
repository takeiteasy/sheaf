;;;; polyrec.lisp
;;;; Polymorphic recursion: each call nests its argument one list deeper.

(defpackage #:sheaf/fixtures/polyrec
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/polyrec)

(coalton-toplevel
  (declare deepen (UFix * :a -> UFix))
  (define (deepen n x)
    (if (== n 0)
        0
        (deepen (- n 1) (make-list x))))

  (declare main (Void -> UFix))
  (define (main)
    (deepen 3 (the UFix 1))))
