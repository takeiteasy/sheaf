;;;; alternating-loops.lisp
;;;; Loops that pass on what an iteration built and drop what they were given:
;;;; one that builds a list, one that threads a parameter through, a pair of
;;;; them and one that builds arrays, then loops that keep what they were given.

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
