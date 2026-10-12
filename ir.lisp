;;;; ir.lisp
;;;; Capturing Coalton's typed codegen IR for a whole program.

(in-package #:sheaf)

(defstruct (program (:constructor %make-program))
  "The codegen IR of a Coalton program, captured before Coalton optimizes it."
  (bindings (make-hash-table :test 'eq) :type hash-table :read-only t))

(defun compile-program (pathname)
  "Compile and load the Coalton source file PATHNAME, capturing the IR of each
binding it defines."
  (let ((program (%make-program)))
    (let ((codegen:*codegen-hook*
            (lambda (category &rest args)
              (when (eq category :codegen-ast)
                (loop :for (name . node) :in (first args)
                      :do (setf (gethash name (program-bindings program)) node))))))
      (uiop:with-temporary-file (:pathname fasl :type (pathname-type (compile-file-pathname "x")))
        (load (or (compile-file pathname :output-file fasl)
                  (error "Failed to compile ~a" pathname)))))
    program))

(defun binding-code (program name)
  "The IR of the global NAME, and whether PROGRAM captured it. A binding
compiled before capture, such as the standard library, has the IR Coalton
kept from before it optimized."
  (multiple-value-bind (node captured) (gethash name (program-bindings program))
    (if captured
        (values node t)
        (values (tc:lookup-source-code entry:*global-environment* name :no-error t) nil))))

(defun global-references (node)
  "The global symbols NODE refers to, in the order first seen."
  (let ((refs '()))
    (flet ((note-variable (n)
             (let ((name (ast:node-variable-value n)))
               (when (tc:lookup-value-type entry:*global-environment* name :no-error t)
                 (pushnew name refs)))
             (values)))
      (traverse:traverse
       node
       (list
        ;; Actions match a node's exact type. Before optimization, a call
        ;; names its global function through a local variable node.
        (traverse:action (:after ast:node-local-variable) #'note-variable)
        (traverse:action (:after ast:node-global-variable) #'note-variable)
        (traverse:action (:after ast:node-direct-application n)
          (pushnew (ast:node-direct-application-rator n) refs)
          (values))))
      (nreverse refs))))

(defun reachable (program root)
  "The globals reachable from ROOT. Returns those with IR, then those without:
constructors, class methods and dictionary constructors, which Coalton defines
from type and class definitions rather than as code."
  (let ((seen (make-hash-table :test 'eq))
        (with-code '())
        (without-code '())
        (stack (list root)))
    (loop :while stack
          :do (let ((name (pop stack)))
                (unless (gethash name seen)
                  (setf (gethash name seen) t)
                  (let ((node (binding-code program name)))
                    (cond (node
                           (push name with-code)
                           (dolist (ref (reverse (global-references node)))
                             (push ref stack)))
                          (t
                           (push name without-code)))))))
    (values (nreverse with-code) (nreverse without-code))))
