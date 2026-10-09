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

  (declare main (Void -> UFix))
  (define (main)
    (let ((h (filled 3)))
      (+ (local-total)
         (+ (made)
            (match (hashtable:get h 3)
              ((Some x) x)
              ((None) 0)))))))
