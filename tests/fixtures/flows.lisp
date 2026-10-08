;;;; flows.lisp
;;;; Functions as type arguments, in a list, bound to a global value and
;;;; used inside their own recursive definition.

(defpackage #:sheaf/fixtures/flows
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/flows)

(coalton-toplevel
  (declare double (UFix -> UFix))
  (define (double x)
    (* x 2))

  (declare adder (UFix -> (UFix -> UFix)))
  (define (adder n)
    (fn (x) (+ x n)))

  (declare inc (UFix -> UFix))
  (define inc (adder 1))

  (declare apply-twice ((UFix -> UFix) * UFix -> UFix))
  (define (apply-twice f x)
    (f (f x)))

  (declare use-option (Optional (UFix -> UFix) * UFix -> UFix))
  (define (use-option o x)
    (match o
      ((Some f) (f x))
      ((None) 0)))

  (declare apply-all (List (UFix -> UFix) * UFix -> UFix))
  (define (apply-all fs x)
    (match fs
      ((Cons f rest) (apply-all rest (f x)))
      ((Nil) x)))

  (declare scaled-sum (UFix * UFix -> UFix))
  (define (scaled-sum n k)
    (let ((go (fn (i acc)
                (if (== i 0)
                    acc
                    (go (- i 1) (+ acc (* i k)))))))
      (go n 0)))

  (declare self-applied (UFix * UFix -> UFix))
  (define (self-applied n k)
    (let ((go (fn (i)
                (if (== i 0)
                    0
                    (+ k (apply-twice go (- i 1)))))))
      (go n)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (use-option (Some double) 4)
       (+ (use-option None 4)
          (+ (apply-all (make-list double (adder 3) inc) 1)
             (+ (inc 5)
                (+ (apply-twice inc 0)
                   (+ (scaled-sum 4 3)
                      (self-applied 3 1)))))))))
