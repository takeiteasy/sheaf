;;;; banked-loops.lisp
;;;; Loops that build a growable value in a bank on every iteration: one that
;;;; keeps none of them, a pair of them, and one that passes the latest on.

(defpackage #:sheaf/fixtures/banked-loops
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/banked-loops)

(coalton-toplevel
  (declare probe (UFix * UFix -> (vector:Vector UFix)))
  (define (probe n acc)
    (let ((v (the (vector:Vector UFix) (vector:new))))
      (vector:push! n v)
      (vector:push! acc v)
      (if (== n 0)
          v
          (probe (- n 1) (+ acc (vector:length v))))))

  (declare ping (UFix * UFix -> (vector:Vector UFix)))
  (define (ping n acc)
    (let ((v (the (vector:Vector UFix) (vector:new))))
      (vector:push! n v)
      (vector:push! acc v)
      (if (== n 0)
          v
          (pong (- n 1) (+ acc 1)))))

  (declare pong (UFix * UFix -> (vector:Vector UFix)))
  (define (pong n acc)
    (let ((v (the (vector:Vector UFix) (vector:new))))
      (vector:push! acc v)
      (vector:push! n v)
      (if (== n 0)
          v
          (ping (- n 1) (+ acc (vector:length v))))))

  (declare carry (UFix * vector:Vector UFix -> (vector:Vector UFix)))
  (define (carry n previous)
    (if (== n 0)
        previous
        (let ((v (the (vector:Vector UFix) (vector:new))))
          (vector:push! n v)
          (vector:push! (vector:length previous) v)
          (carry (- n 1) v))))

  (declare second-of (vector:Vector UFix -> UFix))
  (define (second-of v)
    (vector:index-unsafe 1 v))

  (declare run (UFix -> UFix))
  (define (run n)
    (+ (second-of (probe n 0))
       (+ (second-of (ping n 0))
          (second-of (carry n (the (vector:Vector UFix) (vector:new)))))))

  (declare main (Void -> UFix))
  (define (main)
    (run 10))

  (declare big-probe (Void -> UFix))
  (define (big-probe)
    (second-of (probe 100000 0)))

  (declare big-ping (Void -> UFix))
  (define (big-ping)
    (second-of (ping 100000 0))))
