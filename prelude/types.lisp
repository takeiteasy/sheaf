;;;; types.lisp
;;;; The array and reference types. The operations on them are the primitives
;;;; of Sheaf's IR, so none of their bodies is ever lowered.

(in-package #:sheaf/prelude)

(cl:defvar *lengths* (cl:make-hash-table :test 'cl:eq))

(cl:defun register-length (name n)
  (cl:setf (cl:gethash name *lengths*) n))

(cl:defun fixed-length (name)
  "The length `define-length` declared for the type NAME, or NIL."
  (cl:values (cl:gethash name *lengths*)))

(cl:defstruct slice-internal
  (vector (cl:error "") :type cl:vector)
  (start 0 :type cl:fixnum)
  (end 0 :type cl:fixnum))

(cl:defstruct ref-internal
  (vector (cl:error "") :type cl:vector)
  (index 0 :type cl:fixnum))

(coalton-toplevel
  (repr :native cl:simple-vector)
  (define-type (FixedArray :n :t)
    "An array of the length that :n names, fixed at compile time.")

  (repr :native cl:simple-vector)
  (define-type (Array :t)
    "An array whose length is chosen when it is built.")

  (repr :native (cl:and cl:vector (cl:not cl:simple-vector)))
  (define-type (GrowArray :t)
    "An array that only grows. References into it stay valid.")

  (repr :native (cl:and cl:vector (cl:not cl:simple-vector)))
  (define-type (Stack :t)
    "An array that grows and shrinks.")

  (repr :native slice-internal)
  (define-type (Slice :t)
    "A view of a range of an array.")

  (repr :native ref-internal)
  (define-type (Ref :t)
    "A reference to one element of an array."))
