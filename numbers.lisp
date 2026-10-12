;;;; numbers.lisp
;;;; The numeric types: their C types and what their arithmetic does on overflow.

(in-package #:sheaf)

(defparameter *numeric-types*
  '((coalton:ufix "uint64_t" :wrap 64) (coalton:u64 "uint64_t" :wrap 64)
    (coalton:u32 "uint32_t" :wrap 32) (coalton:u16 "uint16_t" :wrap 16) (coalton:u8 "uint8_t" :wrap 8)
    (coalton:ifix "int64_t" :trap 64) (coalton:integer "int64_t" :trap 64) (coalton:i64 "int64_t" :trap 64)
    (coalton:i32 "int32_t" :trap 32) (coalton:i16 "int16_t" :trap 16) (coalton:i8 "int8_t" :trap 8)
    (coalton:f64 "double" :float 64) (coalton:f32 "float" :float 32))
  "The numeric types: the C type, the kind of overflow, `:wrap` or `:trap`, or
`:float`, and the width in bits of each.")

(defun numeric-entry (name)
  (assoc name *numeric-types*))

(defun numeric-c-type (name)
  (second (numeric-entry name)))

(defun numeric-kind (name)
  (third (numeric-entry name)))

(defun numeric-bits (name)
  (fourth (numeric-entry name)))

(define-condition numeric-overflow (error)
  ((value :initarg :value :reader numeric-overflow-value)
   (type :initarg :type :reader numeric-overflow-type))
  (:report (lambda (condition stream)
             (format stream "Overflow: ~d does not fit ~a"
                     (numeric-overflow-value condition)
                     (symbol-name (numeric-overflow-type condition))))))

(define-condition panic (error)
  ((message :initarg :message :reader panic-message))
  (:report (lambda (condition stream)
             (write-string (panic-message condition) stream))))

(defun float-of (name value)
  (let ((format (if (= (numeric-bits name) 32) 'single-float 'double-float)))
    (cond ((typep value format) value)
          ((ignore-errors (coerce value format)))
          ((minusp value) (if (eq format 'single-float)
                              float-features:single-float-negative-infinity
                              float-features:double-float-negative-infinity))
          (t (if (eq format 'single-float)
                 float-features:single-float-positive-infinity
                 float-features:double-float-positive-infinity)))))

(defun fit (name value)
  "VALUE as a value of the numeric type NAME: reduced to its width if the type
wraps, or converted if it is a float. Signals `numeric-overflow` if a type that
traps cannot hold it."
  (ecase (numeric-kind name)
    (:wrap (mod value (expt 2 (numeric-bits name))))
    (:trap (if (typep value `(signed-byte ,(numeric-bits name)))
               value
               (error 'numeric-overflow :value value :type name)))
    (:float (float-of name value))))
