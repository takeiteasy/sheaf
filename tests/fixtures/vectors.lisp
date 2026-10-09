;;;; vectors.lisp
;;;; Vectors built locally and returned, and every operation on them.

(defpackage #:sheaf/fixtures/vectors
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/vectors)

(coalton-toplevel
  (declare local-total (Void -> UFix))
  (define (local-total)
    (let ((v (the (Vector UFix) (vector:new))))
      (let ((first (vector:push! 4 v)))
        (let ((second (vector:push! 5 v)))
          (vector:push! 6 v)
          (vector:set! 0 10 v)
          (+ (+ first second)
             (+ (vector:index-unsafe 0 v) (vector:length v)))))))

  (declare indexed (Void -> UFix))
  (define (indexed)
    (let ((v (the (Vector UFix) (vector:new))))
      (vector:push! 7 v)
      (+ (match (vector:index 0 v)
           ((Some x) x)
           ((None) 100))
         (match (vector:index 3 v)
           ((Some x) x)
           ((None) 20)))))

  (declare popped (Void -> UFix))
  (define (popped)
    (let ((v (the (Vector UFix) (vector:new))))
      (vector:push! 8 v)
      (vector:push! 9 v)
      (+ (match (vector:pop! v)
           ((Some x) x)
           ((None) 0))
         (vector:length v))))

  (declare filled (UFix -> Vector UFix))
  (define (filled n)
    (let ((v (vector:new)))
      (vector:push! n v)
      (vector:push! (+ n 1) v)
      v))

  (declare main (Void -> UFix))
  (define (main)
    (let ((v (filled 3)))
      (+ (local-total)
         (+ (indexed)
            (+ (popped) (vector:length v)))))))
