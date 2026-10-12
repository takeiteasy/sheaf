;;;; generic-loops.lisp
;;;; A for loop in generic code, carrying a variable of the type variable.

(defpackage #:sheaf/fixtures/generic-loops
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/generic-loops)

(coalton-toplevel
  (declare repeat-last (UFix * :a -> :a))
  (define (repeat-last n x)
    (for ((i 0 (+ i 1))
          (acc x acc))
      :returns acc
      :while (< i n)
      (values)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (repeat-last 3 4) (if (repeat-last 2 True) 1 0))))
