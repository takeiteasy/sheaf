;;;; nested.lisp
;;;; A closure nested inside closures to a depth known only at runtime.

(defpackage #:sheaf/fixtures/nested
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:iter #:coalton/iterator)))

(in-package #:sheaf/fixtures/nested)

(coalton-toplevel
  (declare rewrap (UFix * iter:Iterator UFix -> iter:Iterator UFix))
  (define (rewrap n it)
    (if (== n 0)
        it
        (rewrap (- n 1) (map (fn (x) (+ x 1)) it))))

  (declare main (Void -> UFix))
  (define (main)
    (iter:fold! + 0 (rewrap 3 (iter:up-to 4)))))
