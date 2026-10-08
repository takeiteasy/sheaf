;;;; nested-optional.lisp
;;;; A closure nested inside closures to a runtime depth through an Optional.

(defpackage #:sheaf/fixtures/nested-optional
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/nested-optional)

(coalton-toplevel
  (declare chain (UFix * Optional (UFix -> UFix) -> (UFix -> UFix)))
  (define (chain n previous)
    (if (== n 0)
        (fn (x) x)
        (chain (- n 1)
               (Some (fn (x)
                       (match previous
                         ((Some f) (f (+ x 1)))
                         ((None) x)))))))

  (declare main (Void -> UFix))
  (define (main)
    ((chain 3 None) 0)))
