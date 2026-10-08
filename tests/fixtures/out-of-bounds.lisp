;;;; out-of-bounds.lisp
;;;; An index past the end of an array.

(defpackage #:sheaf/fixtures/out-of-bounds
  (:use #:coalton #:coalton-prelude #:sheaf/prelude)
  (:local-nicknames (#:arr #:sheaf/prelude/arr)))

(in-package #:sheaf/fixtures/out-of-bounds)

(coalton-toplevel
  (declare main (Void -> UFix))
  (define (main)
    (arr:index (arr:new 2 0) 5)))
