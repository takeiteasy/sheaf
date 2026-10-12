;;;; returns.lisp
;;;; An early return from a function: out of a branch, with a list built before
;;;; it, past a Vector built locally, and from a function used as a value.

(defpackage #:sheaf/fixtures/returns
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/returns)

(coalton-toplevel
  (declare early (UFix -> UFix))
  (define (early n)
    (when (> n 5)
      (return 100))
    (let ((m (+ n 1)))
      (if (> m 3)
          (return m)
          (* m 2))))

  (declare len ((List UFix) -> UFix))
  (define (len xs)
    (match xs
      ((Nil) 0)
      ((Cons _ rest) (+ 1 (len rest)))))

  (declare built (UFix -> (List UFix)))
  (define (built n)
    (let ((xs (make-list n (+ n 1))))
      (when (> n 5)
        (return xs))
      (Cons 0 xs)))

  (declare pushed (UFix -> UFix))
  (define (pushed n)
    (let ((v (the (Vector UFix) (vector:new))))
      (vector:push! n v)
      (when (> n 5)
        (return 99))
      (vector:push! 1 v)
      (+ (vector:length v) n)))

  (declare apply-to ((UFix -> UFix) * UFix -> UFix))
  (define (apply-to f x)
    (f x))

  (declare main (Void -> UFix))
  (define (main)
    (+ (+ (early 9) (+ (early 4) (early 1)))
       (+ (+ (len (built 9)) (len (built 2)))
          (+ (+ (pushed 9) (pushed 2))
             (apply-to (fn (x)
                         (when (> x 1)
                           (return 7))
                         x)
                       3))))))
