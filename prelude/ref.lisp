;;;; ref.lisp

(in-package #:sheaf/prelude/ref)

(coalton-toplevel
  (declare get (prelude:Ref :t -> :t))
  (define (get r)
    (lisp (-> :t) (r)
      (cl:aref (prelude::ref-internal-vector r) (prelude::ref-internal-index r))))

  (declare set! (prelude:Ref :t * :t -> Unit))
  (define (set! r x)
    (lisp (-> Unit) (r x)
      (cl:setf (cl:aref (prelude::ref-internal-vector r) (prelude::ref-internal-index r)) x)
      coalton-impl/constants:+value-of-unit+)))
