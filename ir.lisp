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

(defparameter *prelude-replacements*
  '((coalton/vector:find-elem . sheaf/prelude/vector:vector-find-elem)
    (coalton/vector:sort-by! . sheaf/prelude/vector:vector-sort-by!)
    (coalton/vector:with-initial-element . sheaf/prelude/vector:vector-with-initial-element)
    (coalton/hashtable:entries . sheaf/prelude/hashtable:hashtable-entries)
    (coalton/hashtable:keys . sheaf/prelude/hashtable:hashtable-keys)
    (coalton/hashtable:values-iter . sheaf/prelude/hashtable:hashtable-values-iter)
    (coalton/char:code-char . sheaf/prelude/string:char-code-char)
    (coalton/char:ascii-alpha? . sheaf/prelude/string:char-ascii-alpha?)
    (coalton/char:ascii-digit? . sheaf/prelude/string:char-ascii-digit?)
    (coalton/char:ascii-uppercase? . sheaf/prelude/string:char-ascii-uppercase?)
    (coalton/char:ascii-lowercase? . sheaf/prelude/string:char-ascii-lowercase?)
    (coalton/char:digit->ufix . sheaf/prelude/string:char-digit->ufix))
  "The standard library functions whose raw Lisp the prelude replaces with Coalton,
each with the function that stands in for it.")

(defparameter *prelude-method-replacements*
  '((coalton/classes:foldable coalton/vector:vector
     (coalton/classes:fold . sheaf/prelude/vector:vector-fold)
     (coalton/classes:foldr . sheaf/prelude/vector:vector-foldr))
    (coalton/classes:semigroup coalton:string
     (coalton/classes:<> . sheaf/prelude/string:string-append)))
  "The instance methods the prelude replaces: a class, the type whose instance it
is, and each method with the function that stands in for it.")

(defvar *replacement-table* nil)

(defun replacements ()
  "A table from the origin of each standard library function or instance method
the prelude replaces to the function whose code stands in for it."
  (or *replacement-table*
      (setf *replacement-table*
            (let ((table (make-hash-table :test 'eq))
                  (env entry:*global-environment*))
              (loop :for (function . replacement) :in *prelude-replacements*
                    :do (setf (gethash function table) replacement))
              (loop :for (class-name type . methods) :in *prelude-method-replacements*
                    :do (let* ((class (tc:lookup-class env class-name))
                               (names (mapcar #'tc:ty-class-method-name (tc:ty-class-unqualified-methods class))))
                          (dolist (instance (tc:lookup-class-instances env class-name :no-error t))
                            (let ((head (first (tc:flatten-type
                                                (first (tc:ty-predicate-types (tc:ty-class-instance-predicate instance)))))))
                              (when (and (tc:tycon-p head) (eq (tc:tycon-name head) type))
                                (loop :for (method . replacement) :in methods
                                      :do (setf (gethash (nth (position method names)
                                                              (tc:ty-class-instance-method-codegen-syms instance))
                                                         table)
                                                replacement)))))))
              table))))

(defun binding-code (program name)
  "The IR of the global NAME, and whether PROGRAM captured it. A binding
compiled before capture, such as the standard library, has the IR Coalton
kept from before it optimized. A function the prelude replaces has the code of
its replacement."
  (multiple-value-bind (node captured)
      (gethash (gethash name (replacements) name) (program-bindings program))
    (if captured
        (values node t)
        (values (tc:lookup-source-code entry:*global-environment* (gethash name (replacements) name) :no-error t)
                nil))))

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
