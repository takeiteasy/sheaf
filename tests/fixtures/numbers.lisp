;;;; numbers.lisp
;;;; Arithmetic, equality and ordering of each numeric type.

(defpackage #:sheaf/fixtures/numbers
  (:use #:coalton #:coalton-prelude #:sheaf/prelude)
  (:local-nicknames
   (#:arr #:sheaf/prelude/arr)
   (#:hashtable #:coalton/hashtable)))

(in-package #:sheaf/fixtures/numbers)

(coalton-toplevel
  (declare main-u8 (Void -> U8))
  (define (main-u8)
    (+ 250 10))

  (declare main-u16 (Void -> U16))
  (define (main-u16)
    (* 65535 65535))

  (declare main-u32 (Void -> U32))
  (define (main-u32)
    (- 0 1))

  (declare main-u64 (Void -> U64))
  (define (main-u64)
    (- 0 1))

  (declare main-i8 (Void -> I8))
  (define (main-i8)
    (- 5 10))

  (declare main-i16 (Void -> I16))
  (define (main-i16)
    (* 181 181))

  (declare main-i32 (Void -> I32))
  (define (main-i32)
    (* 46340 46340))

  (declare main-i64 (Void -> I64))
  (define (main-i64)
    -9223372036854775808)

  (declare main-ifix (Void -> IFix))
  (define (main-ifix)
    (+ 1 2))

  (declare main-integer (Void -> Integer))
  (define (main-integer)
    (* 3000000000 3))

  (declare main-f64 (Void -> F64))
  (define (main-f64)
    (+ 0.5d0 0.25d0))

  (declare main-f32 (Void -> F32))
  (define (main-f32)
    (* 1.5f0 3.0f0))

  (declare main-array (Void -> F64))
  (define (main-array)
    (let ((a (arr:new 3 1.5d0)))
      (+ (arr:index a 0) (arr:index a 2))))

  (declare main-table (Void -> U8))
  (define (main-table)
    (let ((h (the (hashtable:HashTable UFix U8) (hashtable:new))))
      (hashtable:set! h 1 200)
      (match (hashtable:get h 1)
        ((Some x) x)
        ((None) 0))))

  (declare narrow (Integer -> U8))
  (define (narrow n)
    (fromInt n))

  (declare main-narrow (Void -> U8))
  (define (main-narrow)
    (narrow 300))

  (declare squeeze (Integer -> I8))
  (define (squeeze n)
    (fromInt n))

  (declare main-squeeze (Void -> I8))
  (define (main-squeeze)
    (squeeze 100))

  (declare main-too-wide (Void -> I8))
  (define (main-too-wide)
    (squeeze 200))

  (declare main-big-literal (Void -> I8))
  (define (main-big-literal)
    200)

  (declare bump (I8 -> I8))
  (define (bump x)
    (+ x 1))

  (declare main-overflow (Void -> I8))
  (define (main-overflow)
    (bump 127))

  (declare main-compare (Void -> UFix))
  (define (main-compare)
    (+ (if (< (the U8 3) 5) 1 0)
       (+ (if (> (the I32 -3) -5) 2 0)
          (+ (if (<= (the F64 1.5d0) 1.5d0) 4 0)
             (if (== (the Integer 7) 7) 8 0))))))
