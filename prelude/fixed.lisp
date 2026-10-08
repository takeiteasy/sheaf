;;;; fixed.lisp

(in-package #:sheaf/prelude/fixed)

(coalton-toplevel
  (define-class (Extent :n)
    (new (:t -> prelude:FixedArray :n :t))))

(cl:defmacro define-length (name n)
  "Declare the length type NAME for fixed arrays of N elements. Use it outside
`coalton-toplevel`."
  `(cl:progn
     (cl:eval-when (:compile-toplevel :load-toplevel :execute)
       (sheaf/prelude::register-length ',name ,n))
     (coalton-toplevel
       (define-type ,name)
       (define-instance (Extent ,name)
         (define (new x)
           (lisp (-> (prelude:FixedArray ,name :t)) (x)
             (cl:make-array ,n :initial-element x)))))))
