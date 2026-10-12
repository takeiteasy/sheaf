;;;; panics-uncaught.lisp
;;;; A failed assert that no catch takes.

(defpackage #:sheaf/fixtures/panics-uncaught
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/panics-uncaught)

(coalton-toplevel
  (declare main (Void -> UFix))
  (define (main)
    (assert (> (the UFix 1) 2))
    5))
