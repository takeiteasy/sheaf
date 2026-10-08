;;;; bad-length.lisp
;;;; A length type written by hand, which define-length does not declare.

(defpackage #:sheaf/fixtures/bad-length
  (:use #:coalton #:coalton-prelude #:sheaf/prelude)
  (:local-nicknames (#:fixed #:sheaf/prelude/fixed)
                    (#:arr #:sheaf/prelude/arr)))

(in-package #:sheaf/fixtures/bad-length)

(coalton-toplevel
  (define-type N5)

  (define-instance (fixed:Extent N5)
    (define (fixed:new x)
      (lisp (-> (FixedArray N5 :t)) (x)
        (cl:make-array 5 :initial-element x))))

  (declare main (Void -> UFix))
  (define (main)
    (arr:length (the (FixedArray N5 UFix) (fixed:new 1)))))
