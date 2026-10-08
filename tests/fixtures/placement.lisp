;;;; placement.lisp
;;;; Where storage is built when a result holds only some of it: a fresh list
;;;; from an argument, a view of a parameter, and a value stored into a returned
;;;; array.

(defpackage #:sheaf/fixtures/placement
  (:use #:coalton #:coalton-prelude #:sheaf/prelude)
  (:local-nicknames
   (#:arr #:sheaf/prelude/arr)
   (#:ref #:sheaf/prelude/ref)))

(in-package #:sheaf/fixtures/placement)

(coalton-toplevel
  (declare pair (UFix * UFix -> List UFix))
  (define (pair a b)
    (Cons a (Cons b Nil)))

  (declare total (List UFix -> UFix))
  (define (total xs)
    (match xs
      ((Cons x rest) (+ x (total rest)))
      ((Nil) 0)))

  (declare first-only (List UFix -> List UFix))
  (define (first-only xs)
    (match xs
      ((Cons x _) (Cons x Nil))
      ((Nil) Nil)))

  (declare relay (UFix -> List UFix))
  (define (relay a)
    (first-only (pair a a)))

  (declare second-ref ((Array UFix) -> Ref UFix))
  (define (second-ref a)
    (arr:ref a 1))

  (declare read-second (Void -> UFix))
  (define (read-second)
    (let ((a (arr:new 2 5)))
      (ref:get (second-ref a))))

  (declare filled (UFix -> Array (List UFix)))
  (define (filled a)
    (let ((c (Cons a Nil)))
      (let ((out (arr:new 1 Nil)))
        (arr:set! out 0 c)
        out)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (total (relay 3))
       (+ (read-second)
          (total (arr:index (filled 4) 0))))))
