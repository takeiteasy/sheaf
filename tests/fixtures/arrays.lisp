;;;; arrays.lisp
;;;; Every array and reference type of the prelude, and the operations on them.

(defpackage #:sheaf/fixtures/arrays
  (:use #:coalton #:coalton-prelude #:sheaf/prelude)
  (:local-nicknames
   (#:fixed #:sheaf/prelude/fixed)
   (#:arr #:sheaf/prelude/arr)
   (#:grow #:sheaf/prelude/grow)
   (#:stack #:sheaf/prelude/stack)
   (#:ref #:sheaf/prelude/ref)))

(in-package #:sheaf/fixtures/arrays)

(fixed:define-length N4 4)

(coalton-toplevel
  (declare fixed-total (Void -> UFix))
  (define (fixed-total)
    (let ((a (the (FixedArray N4 UFix) (fixed:new 3))))
      (arr:set! a 1 10)
      (+ (arr:index a 1) (arr:length a))))

  (declare array-total (UFix -> UFix))
  (define (array-total n)
    (let ((a (arr:new n 2)))
      (arr:set! a 0 5)
      (+ (arr:index a 0) (arr:length a))))

  (declare grown (Void -> UFix))
  (define (grown)
    (let ((g (the (GrowArray UFix) (grow:new))))
      (grow:push! g 4)
      (grow:push! g 5)
      (grow:push! g 6)
      (+ (arr:index g 2) (arr:length g))))

  (declare popped (Void -> UFix))
  (define (popped)
    (let ((s (the (Stack UFix) (stack:new))))
      (stack:push! s 7)
      (stack:push! s 8)
      (match (stack:pop! s)
        ((Some x) (+ x (arr:length s)))
        ((None) 0))))

  (declare through-ref (Void -> UFix))
  (define (through-ref)
    (let ((a (the (FixedArray N4 UFix) (fixed:new 1))))
      (let ((r (arr:ref a 2)))
        (ref:set! r 20)
        (+ (ref:get r) (arr:index a 2)))))

  (declare through-slice (Void -> UFix))
  (define (through-slice)
    (let ((g (the (GrowArray UFix) (grow:new))))
      (grow:push! g 1)
      (grow:push! g 2)
      (grow:push! g 3)
      (let ((s (arr:slice g 1 3)))
        (arr:set! s 0 30)
        (let ((r (arr:ref s 1)))
          (+ (+ (arr:index g 1) (ref:get r)) (arr:length s))))))

  (declare two-arrays (Void -> UFix))
  (define (two-arrays)
    (let ((g (the (GrowArray UFix) (grow:new))))
      (let ((f (the (FixedArray N4 UFix) (fixed:new 0))))
        (grow:push! g 1)
        (+ (arr:length g) (arr:index f 0)))))

  (declare main (Void -> UFix))
  (define (main)
    (+ (+ (+ (fixed-total) (array-total 3))
          (+ (grown) (popped)))
       (+ (+ (through-ref) (through-slice)) (two-arrays)))))
