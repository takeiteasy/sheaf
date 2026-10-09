;;;; growables.lisp
;;;; Results that hold growable values, each on a stack of its own.

(defpackage #:sheaf/fixtures/growables
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)
   (#:cell #:coalton/cell)))

(in-package #:sheaf/fixtures/growables)

(coalton-toplevel
  (declare two (UFix -> (Tuple (vector:Vector UFix) (vector:Vector UFix))))
  (define (two n)
    (let ((a (the (vector:Vector UFix) (vector:new)))
          (b (the (vector:Vector UFix) (vector:new))))
      (vector:push! n a)
      (vector:push! n b)
      (Tuple a b)))

  (declare with-list (UFix -> (Tuple (vector:Vector UFix) (List UFix))))
  (define (with-list n)
    (let ((v (the (vector:Vector UFix) (vector:new))))
      (vector:push! n v)
      (Tuple v (Cons n (Cons (+ n 1) Nil)))))

  (declare relay-two (UFix -> (Tuple (vector:Vector UFix) (vector:Vector UFix))))
  (define (relay-two n)
    (two (+ n 1)))

  (declare either (UFix -> (vector:Vector UFix)))
  (define (either n)
    (if (== n 0)
        (let ((v (the (vector:Vector UFix) (vector:new))))
          (vector:push! 1 v)
          v)
        (let ((w (the (vector:Vector UFix) (vector:new))))
          (vector:push! 2 w)
          (vector:push! 3 w)
          w)))

  (declare listed (UFix -> (List (vector:Vector UFix))))
  (define (listed n)
    (let ((a (the (vector:Vector UFix) (vector:new)))
          (b (the (vector:Vector UFix) (vector:new))))
      (vector:push! n a)
      (vector:push! (+ n 1) b)
      (Cons a (Cons b Nil))))

  (declare filled-down (UFix -> (vector:Vector UFix)))
  (define (filled-down n)
    (if (== n 0)
        (let ((v (the (vector:Vector UFix) (vector:new))))
          (vector:push! 7 v)
          v)
        (filled-down (- n 1))))

  (declare vectors-down (UFix -> (List (vector:Vector UFix))))
  (define (vectors-down n)
    (let ((v (the (vector:Vector UFix) (vector:new))))
      (if (== n 0)
          Nil
          (Cons v (vectors-down (- n 1))))))

  (declare vector-count (List (vector:Vector UFix) -> UFix))
  (define (vector-count vectors)
    (match vectors
      ((Nil) 0)
      ((Cons v rest)
       (vector:push! 9 v)
       (+ (vector:length v) (vector-count rest)))))

  (declare main-vectors (Void -> UFix))
  (define (main-vectors)
    (vector-count (vectors-down 3)))

  (declare twos-down (UFix -> (List (Tuple (vector:Vector UFix) (vector:Vector UFix)))))
  (define (twos-down n)
    (if (== n 0)
        Nil
        (Cons (two n) (twos-down (- n 1)))))

  (declare twos-count (List (Tuple (vector:Vector UFix) (vector:Vector UFix)) -> UFix))
  (define (twos-count pairs)
    (match pairs
      ((Nil) 0)
      ((Cons (Tuple a b) rest)
       (vector:push! 9 b)
       (+ (vector:length a) (+ (vector:length b) (twos-count rest))))))

  (declare main-twos (Void -> UFix))
  (define (main-twos)
    (twos-count (twos-down 3)))

  (declare picked (UFix -> UFix))
  (define (picked n)
    (let ((x (if (== n 0)
                 (let ((v (the (vector:Vector UFix) (vector:new))))
                   (vector:push! 1 v)
                   v)
                 (the (vector:Vector UFix) (vector:new)))))
      (vector:push! 9 x)
      (vector:length x)))

  (declare in-cell (UFix -> (cell:Cell (vector:Vector UFix))))
  (define (in-cell n)
    (let ((v (the (vector:Vector UFix) (vector:new))))
      (vector:push! n v)
      (cell:new v)))

  (declare table (vector:Vector UFix))
  (define table
    (let ((v (the (vector:Vector UFix) (vector:new))))
      (vector:push! 1 v)
      (vector:push! 2 v)
      v))

  (declare main-two (Void -> UFix))
  (define (main-two)
    (match (two 3)
      ((Tuple a _)
       (vector:push! 9 a)
       (vector:length a))))

  (declare main-list (Void -> UFix))
  (define (main-list)
    (match (with-list 3)
      ((Tuple v _)
       (vector:push! 9 v)
       (vector:length v))))

  (declare main-relay (Void -> UFix))
  (define (main-relay)
    (match (relay-two 3)
      ((Tuple a b)
       (vector:push! 9 b)
       (+ (vector:length a) (vector:length b)))))

  (declare main-either (Void -> UFix))
  (define (main-either)
    (let ((x (either 0))
          (y (either 1)))
      (vector:push! 9 x)
      (vector:push! 9 y)
      (+ (vector:length x) (vector:length y))))

  (declare main-listed (Void -> UFix))
  (define (main-listed)
    (match (listed 5)
      ((Cons a (Cons b _))
       (vector:push! 9 a)
       (vector:push! 9 b)
       (+ (vector:length a) (vector:length b)))
      (_ 0)))

  (declare main-down (Void -> UFix))
  (define (main-down)
    (let ((v (filled-down 3)))
      (vector:push! 9 v)
      (vector:length v)))

  (declare main-picked (Void -> UFix))
  (define (main-picked)
    (picked 0))

  (declare main-cell (Void -> UFix))
  (define (main-cell)
    (let ((v (cell:read (in-cell 3))))
      (vector:push! 9 v)
      (vector:length v)))

  (declare main-table (Void -> UFix))
  (define (main-table)
    (vector:length table))

  (declare main (Void -> UFix))
  (define (main)
    (+ (main-two)
       (+ (main-list)
          (+ (main-relay)
             (+ (main-either)
                (+ (main-listed)
                   (+ (main-down) (+ (main-picked) (+ (main-cell) (+ (main-table) (+ (main-vectors) (main-twos)))))))))))))
