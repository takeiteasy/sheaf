;;;; lists.lisp
;;;; List functions of the standard library that reverse a list they built.

(defpackage #:sheaf/fixtures/lists
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:list #:coalton/list)))

(in-package #:sheaf/fixtures/lists)

(coalton-toplevel
  (declare total ((List UFix) -> UFix))
  (define (total xs)
    (match xs
      ((Cons x rest) (+ x (total rest)))
      ((Nil) 0)))

  (declare weighted ((List UFix) -> UFix))
  (define (weighted xs)
    (match xs
      ((Cons x rest) (+ x (* 10 (weighted rest))))
      ((Nil) 0)))

  (declare main (Void -> UFix))
  (define (main)
    (let ((xs (make-list 1 2 3 4)))
      (+ (+ (total (map (fn (x) (+ x 1)) xs))
            (weighted (map (fn (x) (* x 2)) xs)))
         (+ (weighted (list:take 3 xs))
            (list:length (list:take 9 xs)))))))
