;;;; for-rejected.lisp
;;;; Loops that are errors: a break out of a catch, and a Vector variable
;;;; built anew by a step.

(defpackage #:sheaf/fixtures/for-rejected
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:vector #:coalton/vector)))

(in-package #:sheaf/fixtures/for-rejected)

(coalton-toplevel
  (define-exception Boom
    (Boom UFix))

  (declare break-in-catch (UFix -> UFix))
  (define (break-in-catch n)
    (for ((i 0 (+ i 1)))
      :returns i
      :while (< i n)
      (catch (progn
               (when (> i 1)
                 (break))
               (throw (Boom i)))
        ((Boom _) (values)))))

  (declare rebuilt (UFix -> UFix))
  (define (rebuilt n)
    (for ((i 0 (+ i 1))
          (v (the (Vector UFix) (vector:new))
             (let ((w (the (Vector UFix) (vector:new))))
               w)))
      :returns (vector:length v)
      :while (< i n)
      (values)))

  (declare main-break (Void -> UFix))
  (define (main-break)
    (break-in-catch 3))

  (declare main-rebuilt (Void -> UFix))
  (define (main-rebuilt)
    (rebuilt 3)))
