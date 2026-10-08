;;;; poly.lisp
;;;; Constrained generics, a constrained instance, superclass access, and a
;;;; local binding used at two types.

(defpackage #:sheaf/fixtures/poly
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:list #:coalton/list)))

(in-package #:sheaf/fixtures/poly)

(coalton-toplevel
  (declare twice (Num :a => :a -> :a))
  (define (twice x)
    (+ x x))

  (declare same? (Num :a => :a * :a -> Boolean))
  (define (same? x y)
    (== (+ x y) y))

  (declare total (Num :a => List :a -> :a))
  (define (total xs)
    (match xs
      ((Cons x rest) (+ x (total rest)))
      ((Nil) 0)))

  (declare main (Void -> UFix))
  (define (main)
    (let ((dbl (fn (z) (+ z z)))
          (doubled (map twice (make-list 1 2 3)))
          (f (dbl (the F32 1.5))))
      (+ (total doubled)
         (+ (if (== (Some (the UFix 1)) (Some 1)) 1 0)
            (+ (if (same? (the UFix 0) 5) 1 0)
               (+ (dbl (twice 1))
                  (+ (list:length Nil)
                     (if (== f (twice (the F32 1.5))) 1 0)))))))))
