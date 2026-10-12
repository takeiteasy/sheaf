;;;; returns-rejected.lisp
;;;; Returns that are errors: out of a catch, and of a growable value.

(defpackage #:sheaf/fixtures/returns-rejected
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/returns-rejected)

(coalton-toplevel
  (define-exception Boom
    (Boom UFix))

  (declare across-catch (UFix -> UFix))
  (define (across-catch n)
    (catch (progn
             (when (> n 1)
               (return 5))
             (throw (Boom n)))
      ((Boom x) x)))

  (declare grown (UFix -> (Vector UFix)))
  (define (grown n)
    (let ((v (the (Vector UFix) (vector:new))))
      (vector:push! n v)
      (when (> n 5)
        (return v))
      v))

  (declare main-catch (Void -> UFix))
  (define (main-catch)
    (across-catch 3))

  (declare main-grown (Void -> UFix))
  (define (main-grown)
    (vector:length (grown 3))))
