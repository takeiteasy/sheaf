;;;; grow.lisp

(in-package #:sheaf/prelude/grow)

(coalton-toplevel
  (declare new (Void -> prelude:GrowArray :t))
  (define (new)
    (lisp (-> (prelude:GrowArray :t)) ()
      (cl:make-array 0 :adjustable cl:t :fill-pointer 0)))

  (declare push! (prelude:GrowArray :t * :t -> Unit))
  (define (push! a x)
    (lisp (-> Unit) (a x)
      (cl:vector-push-extend x a)
      coalton-impl/constants:+value-of-unit+)))
