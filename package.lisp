;;;; package.lisp

(defpackage #:sheaf
  (:use #:cl)
  (:local-nicknames
   (#:ast #:coalton-impl/codegen/ast)
   (#:codegen #:coalton-impl/codegen)
   (#:entry #:coalton-impl/entry)
   (#:immutable-map #:coalton-impl/algorithm/immutable-map)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker)
   (#:traverse #:coalton-impl/codegen/traverse)
   (#:typecheck #:coalton-impl/codegen/typecheck-node))
  (:export
   #:program
   #:program-bindings
   #:compile-program
   #:binding-code
   #:global-references
   #:reachable
   #:*specialization-limit*
   #:specialization-error
   #:specialization-error-chain
   #:specialized
   #:specialized-bindings
   #:specialized-root
   #:specialized-origin
   #:monomorphize
   #:check-monomorphic))
