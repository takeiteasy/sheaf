;;;; table-iterators.lisp
;;;; The keys, values and entries of a HashTable, and a table filled from another.

(defpackage #:sheaf/fixtures/table-iterators
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:hashtable #:coalton/hashtable)
   (#:iter #:coalton/iterator)))

(in-package #:sheaf/fixtures/table-iterators)

(coalton-toplevel
  (declare filled (Void -> (hashtable:HashTable UFix UFix)))
  (define (filled)
    (let ((h (the (hashtable:HashTable UFix UFix) (hashtable:new))))
      (hashtable:set! h 3 30)
      (hashtable:set! h 5 50)
      (hashtable:set! h 9 90)
      (hashtable:set! h 3 31)
      h))

  (declare key-sum (Void -> UFix))
  (define (key-sum)
    (iter:sum! (hashtable:keys (filled))))

  (declare value-sum (Void -> UFix))
  (define (value-sum)
    (iter:sum! (hashtable:values-iter (filled))))

  (declare pair-sum (Void -> UFix))
  (define (pair-sum)
    (iter:sum! (map (fn ((Tuple k v)) (+ (* k 1000) v))
                    (hashtable:entries (filled)))))

  (declare empty-count (Void -> UFix))
  (define (empty-count)
    (iter:count! (hashtable:keys (the (hashtable:HashTable UFix UFix) (hashtable:new)))))

  (declare copied (Void -> UFix))
  (define (copied)
    (let ((target (the (hashtable:HashTable UFix UFix) (hashtable:new))))
      (hashtable:extend! target (hashtable:entries (filled)))
      (+ (match (hashtable:get target 5) ((Some x) x) ((None) 0))
         (the UFix (fromInt (hashtable:count target))))))

  (declare main (Void -> UFix))
  (define (main)
    (+ (+ (key-sum) (value-sum))
       (+ (pair-sum) (+ (empty-count) (copied))))))
