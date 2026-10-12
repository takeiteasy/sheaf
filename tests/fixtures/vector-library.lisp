;;;; vector-library.lisp
;;;; The Vector functions that copy, cut, reverse and resize a vector.

(defpackage #:sheaf/fixtures/vector-library
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/vector-library)

(coalton-toplevel
  (declare filled (Void -> (Vector UFix)))
  (define (filled)
    (let ((v (the (Vector UFix) (vector:with-capacity 10))))
      (vector:push! 1 v)
      (vector:push! 2 v)
      (vector:push! 3 v)
      (vector:push! 4 v)
      (vector:push! 5 v)
      v))

  (declare sum-of ((Vector UFix) -> UFix))
  (define (sum-of v)
    (let ((total (the UFix 0)))
      (for ((i 0 (+ i 1))
            (acc 0 (+ acc (vector:index-unsafe i v))))
        :returns acc
        :while (< i (vector:length v))
        (values))))

  (declare copied (Void -> UFix))
  (define (copied)
    (let ((v (filled)))
      (let ((c (vector:copy v)))
        (vector:push! 100 c)
        (+ (+ (sum-of v) (sum-of c))
           (vector:length c)))))

  (declare cut (Void -> UFix))
  (define (cut)
    (let ((v (filled)))
      (let ((s (vector:subseq v 1 4)))
        (let ((reversed (vector:subseq v 4 1)))
          (let ((past (vector:subseq v 3 99)))
            (+ (+ (sum-of s) (* 10 (sum-of reversed)))
               (+ (* 100 (sum-of past)) (* 1000 (vector:length past)))))))))

  (declare reversed (Void -> UFix))
  (define (reversed)
    (let ((v (filled)))
      (vector:reverse! v)
      (let ((empty (the (Vector UFix) (vector:new))))
        (vector:reverse! empty)
        (+ (+ (vector:index-unsafe 0 v) (* 10 (vector:index-unsafe 4 v)))
           (+ (* 100 (vector:index-unsafe 1 v)) (vector:length empty))))))

  (declare resized (Void -> UFix))
  (define (resized)
    (let ((v (filled)))
      (vector:resect! v 1 3)
      (let ((c (filled)))
        (vector:set-capacity! 2 c)
        (vector:set-capacity! 20 c)
        (+ (+ (sum-of v) (* 10 (vector:length v)))
           (+ (* 100 (sum-of c))
              (if (< (vector:capacity c) (vector:length c)) 1000 0))))))

  (declare popped (Void -> UFix))
  (define (popped)
    (let ((v (filled)))
      (+ (vector:pop-unsafe! v)
         (* 10 (vector:pop-unsafe! v)))))

  (declare main (Void -> UFix))
  (define (main)
    (+ (+ (copied) (cut))
       (+ (+ (reversed) (resized)) (popped)))))
