;;;; hashtables.lisp
;;;; Hashtables built locally and returned, and every operation on them.

(defpackage #:sheaf/fixtures/hashtables
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:hashtable #:coalton/hashtable)))

(in-package #:sheaf/fixtures/hashtables)

(coalton-toplevel
  (declare local-total (Void -> UFix))
  (define (local-total)
    (let ((h (the (hashtable:HashTable UFix UFix) (hashtable:new))))
      (hashtable:set! h 1 10)
      (hashtable:set! h 2 20)
      (hashtable:set! h 1 11)
      (hashtable:remove! h 2)
      (+ (match (hashtable:get h 1)
           ((Some x) x)
           ((None) 100))
         (match (hashtable:get h 2)
           ((Some x) x)
           ((None) 5)))))

  (declare made (Void -> UFix))
  (define (made)
    (let ((h (the (hashtable:HashTable UFix UFix) (hashtable:make (3 30) (4 40)))))
      (+ (match (hashtable:get h 3)
           ((Some x) x)
           ((None) 0))
         (match (hashtable:get h 4)
           ((Some x) x)
           ((None) 0)))))

  (declare filled (UFix -> hashtable:HashTable UFix UFix))
  (define (filled n)
    (let ((h (hashtable:new)))
      (hashtable:set! h n (+ n 1))
      h))

  (declare counted (Void -> Integer))
  (define (counted)
    (let ((h (filled 3)))
      (hashtable:set! h 9 9)
      (hashtable:count h)))

  (declare boolean-keys (Void -> UFix))
  (define (boolean-keys)
    (let ((h (the (hashtable:HashTable Boolean UFix) (hashtable:new))))
      (hashtable:set! h True 1)
      0))

  (declare fill-to (UFix * (hashtable:HashTable UFix UFix) -> Unit))
  (define (fill-to n h)
    (if (== n 0)
        Unit
        (progn
          (hashtable:set! h (- n 1) n)
          (fill-to (- n 1) h))))

  (declare drop-evens (UFix * (hashtable:HashTable UFix UFix) -> Unit))
  (define (drop-evens n h)
    (if (== n 0)
        Unit
        (progn
          (hashtable:remove! h (* 2 (- n 1)))
          (drop-evens (- n 1) h))))

  (declare sum-from (UFix * UFix * (hashtable:HashTable UFix UFix) -> UFix))
  (define (sum-from n acc h)
    (if (== n 0)
        acc
        (sum-from (- n 1)
                  (+ acc (match (hashtable:get h (- n 1))
                           ((Some x) x)
                           ((None) 0)))
                  h)))

  (declare main-many (Void -> UFix))
  (define (main-many)
    (let ((h (the (hashtable:HashTable UFix UFix) (hashtable:new))))
      (fill-to 100 h)
      (drop-evens 50 h)
      (sum-from 100 0 h)))

  (declare tables-down (UFix -> (List (hashtable:HashTable UFix UFix))))
  (define (tables-down n)
    (let ((h (the (hashtable:HashTable UFix UFix) (hashtable:new))))
      (hashtable:set! h n n)
      (if (== n 0)
          Nil
          (Cons h (tables-down (- n 1))))))

  (declare table-total (List (hashtable:HashTable UFix UFix) -> UFix))
  (define (table-total tables)
    (match tables
      ((Nil) 0)
      ((Cons h rest)
       (hashtable:set! h 100 1)
       (+ (match (hashtable:get h 100)
            ((Some x) x)
            ((None) 0))
          (table-total rest)))))

  (declare sized (Integer -> UFix))
  (define (sized n)
    (let ((h (the (hashtable:HashTable UFix UFix) (hashtable:with-capacity (* n 2)))))
      (hashtable:set! h 1 7)
      (match (hashtable:get h 1)
        ((Some x) x)
        ((None) 0))))

  (declare main-sized (Void -> UFix))
  (define (main-sized)
    (sized 4))

  (declare main-tables (Void -> UFix))
  (define (main-tables)
    (table-total (tables-down 3)))

  (declare main (Void -> UFix))
  (define (main)
    (let ((h (filled 3)))
      (+ (local-total)
         (+ (made)
            (match (hashtable:get h 3)
              ((Some x) x)
              ((None) 0)))))))
