;;;; stack.lisp

(in-package #:sheaf/prelude/stack)

(coalton-toplevel
  (declare new (Void -> prelude:Stack :t))
  (define (new)
    (lisp (-> (prelude:Stack :t)) ()
      (cl:make-array 0 :adjustable cl:t :fill-pointer 0)))

  (declare push! (prelude:Stack :t * :t -> Unit))
  (define (push! a x)
    (lisp (-> Unit) (a x)
      (cl:vector-push-extend x a)
      coalton-impl/constants:+value-of-unit+))

  (declare pop! (prelude:Stack :t -> Optional :t))
  (define (pop! a)
    (if (lisp (-> Boolean) (a) (cl:zerop (cl:length a)))
        None
        (Some (lisp (-> :t) (a) (cl:vector-pop a))))))
