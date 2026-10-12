;;;; panics-rejected.lisp
;;;; Raw Lisp that has no translation.

(defpackage #:sheaf/fixtures/panics-rejected
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/panics-rejected)

(coalton-toplevel
  (declare main-format (Void -> UFix))
  (define (main-format)
    (assert (> (the UFix 1) 2) "one is ~a" 1)
    5)

  (declare main-raw (Void -> UFix))
  (define (main-raw)
    (lisp (-> UFix) () (cl:+ 1 2))))
