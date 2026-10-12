;;;; constructors.lisp
;;;; A constructor passed to a function, stored in a variable and applied directly.

(defpackage #:sheaf/fixtures/constructors
  (:use #:coalton #:coalton-prelude))

(in-package #:sheaf/fixtures/constructors)

(coalton-toplevel
  (define-type Box
    (Box UFix))

  (define-type Pair
    (Pair UFix UFix))

  (declare unbox (Box -> UFix))
  (define (unbox b)
    (match b
      ((Box x) x)))

  (declare build ((UFix -> Box) * UFix -> Box))
  (define (build f x)
    (f x))

  (declare sum-boxes ((List Box) -> UFix))
  (define (sum-boxes boxes)
    (fold (fn (acc b) (+ acc (unbox b))) 0 boxes))

  (declare swap-sum (UFix * UFix -> UFix))
  (define (swap-sum a b)
    (let ((make (fn (x y) (Pair y x))))
      (match (make a b)
        ((Pair first second) (+ (* first 10) second)))))

  (declare main (Void -> UFix))
  (define (main)
    (let ((stored Box))
      (+ (sum-boxes (map Box (make-list 1 2 3)))
         (+ (unbox (build Box 4))
            (+ (unbox (stored 5))
               (+ (swap-sum 1 2)
                  (unbox (Box 6)))))))))
