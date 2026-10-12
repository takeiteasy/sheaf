;;;; extend.lisp
;;;; Vectors and hashtables filled from an iterator: extend!, append,
;;;; with-initial-element and a table built from a list of pairs.

(defpackage #:sheaf/fixtures/extend
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)
   (#:hashtable #:coalton/hashtable)))

(in-package #:sheaf/fixtures/extend)

(coalton-toplevel
  (declare digits ((Vector UFix) -> UFix))
  (define (digits v)
    (fold (fn (acc x) (+ (* acc 10) x)) 0 v))

  (declare extended (Void -> UFix))
  (define (extended)
    (let ((v (the (Vector UFix) (vector:new))))
      (vector:push! 9 v)
      (vector:extend! v (make-list 1 2 3))
      (digits v)))

  (declare appended (Void -> UFix))
  (define (appended)
    (let ((a (the (Vector UFix) (vector:new)))
          (b (the (Vector UFix) (vector:new))))
      (vector:push! 1 a)
      (vector:push! 2 b)
      (vector:push! 3 b)
      (let ((c (vector:append a b)))
        (+ (digits c) (* 1000 (vector:length c))))))

  (declare repeated (Void -> UFix))
  (define (repeated)
    (digits (vector:with-initial-element 4 7)))

  (declare tabled (Void -> UFix))
  (define (tabled)
    (let ((h (the (hashtable:HashTable UFix UFix) (hashtable:new))))
      (hashtable:extend! h (make-list (Tuple 1 10) (Tuple 2 20) (Tuple 1 30)))
      (+ (the UFix (fromInt (hashtable:count h)))
         (+ (match (hashtable:get h 1) ((Some x) x) ((None) 0))
            (match (hashtable:get h 2) ((Some x) x) ((None) 0))))))

  (declare main (Void -> UFix))
  (define (main)
    (+ (+ (extended) (appended))
       (+ (repeated) (tabled)))))
