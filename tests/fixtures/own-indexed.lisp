;;;; own-indexed.lisp
;;;; A program's own instance of the prelude's Indexed class, which stays an
;;;; ordinary function.

(defpackage #:sheaf/fixtures/own-indexed
  (:use #:coalton #:coalton-prelude #:sheaf/prelude)
  (:local-nicknames (#:arr #:sheaf/prelude/arr)))

(in-package #:sheaf/fixtures/own-indexed)

(coalton-toplevel
  (define-type (Box :a)
    (Box :a))

  (define-instance (arr:Indexed Box)
    (define (arr:length _) 1)
    (define (arr:index b _)
      (match b
        ((Box x) x)))
    (define (arr:set! _ _ _) Unit))

  (declare main (Void -> UFix))
  (define (main)
    (+ (arr:index (Box 5) 0) (arr:length (Box 1)))))
