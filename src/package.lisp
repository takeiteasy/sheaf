;;;; package.lisp

(defpackage #:sheaf
  (:use #:cl)
  (:local-nicknames
   (#:ast #:coalton-impl/codegen/ast)
   (#:codegen #:coalton-impl/codegen)
   (#:entry #:coalton-impl/entry)
   (#:tc #:coalton-impl/typechecker)
   (#:traverse #:coalton-impl/codegen/traverse))
  (:export
   #:program
   #:program-bindings
   #:compile-program
   #:binding-code
   #:global-references
   #:reachable))
