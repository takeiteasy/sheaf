;;;; recursive-data.lisp
;;;; A declared recursive type holding functions, and a list of closures
;;;; that capture the list. Neither nests a closure inside itself by value.

(defpackage #:sheaf/fixtures/recursive-data
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/recursive-data)

(coalton-toplevel
  (define-type Chain
    (Link (UFix -> UFix) Chain)
    End)

  (declare run-chain (Chain * UFix -> UFix))
  (define (run-chain chain x)
    (match chain
      ((Link f rest) (run-chain rest (f x)))
      ((End) x)))

  (declare run-all (List (UFix -> UFix) * UFix -> UFix))
  (define (run-all fs x)
    (match fs
      ((Cons f rest) (run-all rest (f x)))
      ((Nil) x)))

  (declare collect (UFix * List (UFix -> UFix) -> List (UFix -> UFix)))
  (define (collect n fs)
    (if (== n 0)
        fs
        (collect (- n 1) (Cons (fn (x) (+ n (run-all fs x))) fs))))

  (declare main (Void -> UFix))
  (define (main)
    (+ (run-chain (Link (fn (x) (+ x 1)) (Link (fn (x) (* x 2)) End)) 1)
       (run-all (collect 3 Nil) 0))))
