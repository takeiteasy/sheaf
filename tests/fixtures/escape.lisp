;;;; escape.lisp
;;;; A function passed to raw Lisp, which cannot be analyzed.

(defpackage #:sheaf/fixtures/escape
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/escape)

(coalton-toplevel
  (declare call-raw ((UFix -> UFix) -> UFix))
  (define (call-raw f)
    (lisp (-> UFix) (f)
      (cl:funcall f 1)))

  (declare main (Void -> UFix))
  (define (main)
    (call-raw (fn (x) (+ x 1)))))
