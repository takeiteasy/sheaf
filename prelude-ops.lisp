;;;; prelude-ops.lisp
;;;; The prelude functions, instance methods and types that are IR primitives.

(in-package #:sheaf)

;;; The prelude's array and reference operations are primitives too. Each
;;; operation is its function's or its instance method's origin.

(defparameter *prelude-types*
  '((sheaf/prelude:fixedarray :storage :fixed)
    (sheaf/prelude:array :storage :variable)
    (sheaf/prelude:growarray :storage :resizable)
    (sheaf/prelude:stack :storage :resizable)
    (sheaf/prelude:slice :view)
    (sheaf/prelude:ref :view)
    (coalton/cell:cell :storage :fixed)
    (coalton/vector:vector :storage :resizable)
    (coalton/hashtable:hashtable :storage :resizable))
  "The prelude's types: the kind of each, and for storage its size class.")

(defparameter *prelude-ops*
  '((:fixed-new 1 :allocates) (:array-new 2 :allocates) (:grow-new 0 :allocates) (:stack-new 0 :allocates)
    (:cell-new 1 :allocates) (:vector-new 0 :allocates)
    (:hashtable-new 0 :allocates) (:hashtable-with-capacity 1 :allocates)
    (:hashtable-get 2) (:hashtable-set 3) (:hashtable-remove 2) (:hashtable-count 1)
    (:grow-push 2) (:stack-push 2) (:stack-pop 1)
    (:ref-get 1) (:ref-set 2)
    (:cell-read 1) (:cell-write 2) (:cell-swap 2)
    (:vector-push 2) (:vector-pop 1) (:vector-index 2) (:vector-index-unsafe 2) (:vector-set 3)
    (:vector-length 1)
    (:vector-with-capacity 1 :allocates) (:vector-capacity 1) (:vector-set-capacity 2)
    (:vector-pop-unsafe 1) (:vector-copy 1 :allocates) (:vector-subseq 3 :allocates)
    (:vector-resect 3) (:vector-reverse 1)
    (:elements-length 1) (:elements-index 2) (:elements-set 3)
    (:elements-ref 2) (:elements-slice 3))
  "The operations of the prelude: the arity of each, and whether it allocates.")

(defparameter *prelude-functions*
  '((sheaf/prelude/arr:new . :array-new)
    (sheaf/prelude/grow:new . :grow-new)
    (sheaf/prelude/grow:push! . :grow-push)
    (sheaf/prelude/stack:new . :stack-new)
    (sheaf/prelude/stack:push! . :stack-push)
    (sheaf/prelude/stack:pop! . :stack-pop)
    (sheaf/prelude/ref:get . :ref-get)
    (sheaf/prelude/ref:set! . :ref-set)
    (coalton/cell:new . :cell-new)
    (coalton/cell:read . :cell-read)
    (coalton/cell:write! . :cell-write)
    (coalton/cell:swap! . :cell-swap)
    (coalton/vector:new . :vector-new)
    (coalton/vector:push! . :vector-push)
    (coalton/vector:pop! . :vector-pop)
    (coalton/vector:index . :vector-index)
    (coalton/vector:index-unsafe . :vector-index-unsafe)
    (coalton/vector:set! . :vector-set)
    (coalton/vector:length . :vector-length)
    (coalton/vector:with-capacity . :vector-with-capacity)
    (coalton/vector:capacity . :vector-capacity)
    (coalton/vector:set-capacity! . :vector-set-capacity)
    (coalton/vector:pop-unsafe! . :vector-pop-unsafe)
    (coalton/vector:copy . :vector-copy)
    (coalton/vector:subseq . :vector-subseq)
    (coalton/vector:resect! . :vector-resect)
    (coalton/vector:reverse! . :vector-reverse)
    (coalton/hashtable:new . :hashtable-new)
    (coalton/hashtable:with-capacity . :hashtable-with-capacity)
    (coalton/hashtable:get . :hashtable-get)
    (coalton/hashtable:set! . :hashtable-set)
    (coalton/hashtable:remove! . :hashtable-remove)
    (coalton/hashtable:count . :hashtable-count)))

(defparameter *prelude-classes*
  '((sheaf/prelude/fixed:extent :fixed-new)
    (sheaf/prelude/arr:indexed :elements-length :elements-index :elements-set)
    (sheaf/prelude/arr:viewable :elements-ref :elements-slice))
  "The classes whose instance methods are primitives, and the operation of each
method in the order the class declares them.")

(defvar *prelude-primitives* (make-hash-table :test 'eq)
  "The prelude's operations by origin, for the program being lowered.")

(defun prelude-instance-p (instance)
  "Whether INSTANCE is of a type of the prelude, not one a program defines."
  (let ((head (first (tc:flatten-type
                      (first (tc:ty-predicate-types (tc:ty-class-instance-predicate instance)))))))
    (and (tc:tycon-p head) (assoc (tc:tycon-name head) *prelude-types*) t)))

(defun prelude-primitives ()
  (let ((table (make-hash-table :test 'eq)))
    (loop :for (function . op) :in *prelude-functions*
          :do (setf (gethash function table) op))
    (loop :for (class . ops) :in *prelude-classes*
          :do (dolist (instance (tc:lookup-class-instances entry:*global-environment* class :no-error t))
                (when (or (eq class 'sheaf/prelude/fixed:extent) (prelude-instance-p instance))
                  (loop :for symbol :in (tc:ty-class-instance-method-codegen-syms instance)
                        :for op :in ops
                        :do (setf (gethash symbol table) op)))))
    table))
