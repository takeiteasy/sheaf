;;;; alternating-loops.lisp
;;;; Loops that pass on what an iteration built and drop what they were given:
;;;; one that builds a list, one that threads a parameter through, a pair of
;;;; them and one that builds arrays, then loops that keep what they were given.
;;;; Loops that return storage they built, which is copied out when they return.

(defpackage #:sheaf/fixtures/alternating-loops
  (:use #:coalton #:coalton-prelude #:sheaf/prelude)
  (:local-nicknames
   (#:arr #:sheaf/prelude/arr)))

(in-package #:sheaf/fixtures/alternating-loops)

(coalton-toplevel
  (declare total (List UFix -> UFix))
  (define (total xs)
    (match xs
      ((Cons x rest) (+ x (total rest)))
      ((Nil) 0)))

  (declare rebuild (UFix * List UFix -> UFix))
  (define (rebuild n xs)
    (if (== n 0)
        (total xs)
        (rebuild (- n 1) (Cons n Nil))))

  (declare thread (UFix * List UFix * List UFix -> UFix))
  (define (thread n env xs)
    (if (== n 0)
        (+ (total xs) (total env))
        (thread (- n 1) env (Cons (+ n (total env)) (Cons (total xs) Nil)))))

  (declare ping (UFix * List UFix -> UFix))
  (define (ping n xs)
    (if (== n 0)
        (total xs)
        (pong (- n 1) (Cons n Nil))))

  (declare pong (UFix * List UFix -> UFix))
  (define (pong n xs)
    (if (== n 0)
        (total xs)
        (ping (- n 1) (Cons (+ n (total xs)) Nil))))

  (declare spin (UFix * Array UFix -> UFix))
  (define (spin n a)
    (if (== n 0)
        (arr:index a 0)
        (spin (- n 1) (arr:new 3 n))))

  (declare accumulate (UFix * List UFix -> UFix))
  (define (accumulate n xs)
    (if (== n 0)
        (total xs)
        (accumulate (- n 1) (Cons n xs))))

  (declare sum-down (UFix * List UFix -> UFix))
  (define (sum-down n xs)
    (if (== n 0)
        (total xs)
        (+ 1 (sum-down (- n 1) (Cons n Nil)))))

  (declare rebuild-list (UFix * List UFix -> List UFix))
  (define (rebuild-list n xs)
    (if (== n 0)
        xs
        (rebuild-list (- n 1) (Cons n Nil))))

  (declare thread-list (UFix * List UFix * List UFix -> List UFix))
  (define (thread-list n env xs)
    (if (== n 0)
        (Cons (total xs) env)
        (thread-list (- n 1) env (Cons (+ n (total env)) (Cons (total xs) Nil)))))

  (declare ping-list (UFix * List UFix -> List UFix))
  (define (ping-list n xs)
    (if (== n 0)
        xs
        (pong-list (- n 1) (Cons n Nil))))

  (declare pong-list (UFix * List UFix -> List UFix))
  (define (pong-list n xs)
    (if (== n 0)
        xs
        (ping-list (- n 1) (Cons (+ n (total xs)) Nil))))

  (declare pair-up (UFix * List UFix * List UFix -> (Tuple (List UFix) (List UFix))))
  (define (pair-up n env xs)
    (if (== n 0)
        (Tuple xs env)
        (pair-up (- n 1) env (Cons n Nil))))

  (declare pair-total (UFix -> UFix))
  (define (pair-total n)
    (match (pair-up n (Cons 5 (Cons 6 Nil)) Nil)
      ((Tuple a b) (+ (total a) (total b)))))

  (declare spin-array (UFix * Array UFix -> Array UFix))
  (define (spin-array n a)
    (if (== n 0)
        a
        (spin-array (- n 1) (arr:new 3 n))))

  (declare run-lists (UFix -> UFix))
  (define (run-lists n)
    (+ (total (rebuild-list n Nil))
       (+ (total (thread-list n (Cons 5 (Cons 6 Nil)) Nil))
          (+ (total (ping-list n Nil))
             (+ (pair-total n)
                (arr:index (spin-array n (arr:new 3 0)) 0))))))

  (declare main-lists (Void -> UFix))
  (define (main-lists)
    (run-lists 10))

  (declare big-lists (Void -> UFix))
  (define (big-lists)
    (+ (total (rebuild-list 1000000 Nil))
       (+ (total (thread-list 1000000 (Cons 5 (Cons 6 Nil)) Nil))
          (+ (total (ping-list 1000000 Nil))
             (pair-total 1000000)))))

  (declare run (UFix -> UFix))
  (define (run n)
    (+ (rebuild n Nil)
       (+ (thread n (Cons 5 (Cons 6 Nil)) Nil)
          (+ (ping n Nil)
             (+ (accumulate n Nil)
                (+ (sum-down n Nil)
                   (spin n (arr:new 3 0))))))))

  (declare main (Void -> UFix))
  (define (main)
    (run 10))

  (declare big (Void -> UFix))
  (define (big)
    (+ (rebuild 1000000 Nil)
       (+ (thread 1000000 (Cons 5 (Cons 6 Nil)) Nil)
          (+ (ping 1000000 Nil)
             (spin 1000000 (arr:new 3 0)))))))
