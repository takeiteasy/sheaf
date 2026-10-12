;;;; hashtable.lisp
;;;; Iterators over a HashTable, whose standard library code is raw Lisp, as a
;;;; cursor over the table's slots. The table functions are primitives: on
;;;; Coalton's backend a slot is a position in the table's order.

(in-package #:sheaf/prelude/hashtable)

(coalton-toplevel
  (declare table-next-slot (hashtable:HashTable :key :value * UFix -> Optional UFix))
  (define (table-next-slot table slot)
    (if (< slot (the UFix (fromInt (hashtable:count table))))
        (Some slot)
        None))

  (declare table-slot-key (hashtable:HashTable :key :value * UFix -> :key))
  (define (table-slot-key table slot)
    (lisp (-> :key) (table slot)
      (cl:let ((next (shim:custom-hash-table-iter table)))
        (cl:dotimes (skipped slot)
          (cl:funcall next))
        (cl:multiple-value-bind (presentp key value) (cl:funcall next)
          (cl:declare (cl:ignore presentp value))
          key))))

  (declare table-slot-value (hashtable:HashTable :key :value * UFix -> :value))
  (define (table-slot-value table slot)
    (lisp (-> :value) (table slot)
      (cl:let ((next (shim:custom-hash-table-iter table)))
        (cl:dotimes (skipped slot)
          (cl:funcall next))
        (cl:multiple-value-bind (presentp key value) (cl:funcall next)
          (cl:declare (cl:ignore presentp key))
          value))))

  (declare hashtable-entries (hashtable:HashTable :key :value -> iter:Iterator (Tuple :key :value)))
  (define (hashtable-entries table)
    (let ((cursor (cell:new 0)))
      (iter:new
       (fn ()
         (match (table-next-slot table (cell:read cursor))
           ((Some slot)
            (cell:write! cursor (+ slot 1))
            (Some (Tuple (table-slot-key table slot) (table-slot-value table slot))))
           ((None) None))))))

  (declare hashtable-keys (hashtable:HashTable :key :value -> iter:Iterator :key))
  (define (hashtable-keys table)
    (let ((cursor (cell:new 0)))
      (iter:new
       (fn ()
         (match (table-next-slot table (cell:read cursor))
           ((Some slot)
            (cell:write! cursor (+ slot 1))
            (Some (table-slot-key table slot)))
           ((None) None))))))

  (declare hashtable-values-iter (hashtable:HashTable :key :value -> iter:Iterator :value))
  (define (hashtable-values-iter table)
    (let ((cursor (cell:new 0)))
      (iter:new
       (fn ()
         (match (table-next-slot table (cell:read cursor))
           ((Some slot)
            (cell:write! cursor (+ slot 1))
            (Some (table-slot-value table slot)))
           ((None) None)))))))
