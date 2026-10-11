;;;; exceptions-rejected.lisp
;;;; Exceptions that hold storage or link cells, which a throw cannot copy, and handlers that run before unwinding.

(defpackage #:sheaf/fixtures/exceptions-rejected
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/exceptions-rejected)

(coalton-toplevel
  (define-exception Heavy
    (Heavy (vector:Vector UFix)))

  (define-exception Linked
    (Linked (List UFix)))

  (declare throw-vector (Void -> UFix))
  (define (throw-vector)
    (throw (Heavy (the (vector:Vector UFix) (vector:new)))))

  (declare throw-list (Void -> UFix))
  (define (throw-list)
    (throw (Linked (Cons 1 Nil))))

  (define-exception Skip
    (Skip))

  (define-resumption Retry)

  (declare main-handle (Void -> UFix))
  (define (main-handle)
    (handle (the UFix (throw Skip))
      ((Skip) 1)))

  (declare main-resumable (Void -> UFix))
  (define (main-resumable)
    (resumable 1
      ((Retry) 2)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (throw-vector) (throw-list))))
