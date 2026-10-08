;;;; package.lisp

(defpackage #:sheaf
  (:use #:cl)
  (:local-nicknames
   (#:ast #:coalton-impl/codegen/ast)
   (#:codegen #:coalton-impl/codegen)
   (#:entry #:coalton-impl/entry)
   (#:immutable-map #:coalton-impl/algorithm/immutable-map)
   (#:pattern #:coalton-impl/codegen/pattern)
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
   #:specialization-error-message
   #:specialized
   #:specialized-bindings
   #:specialized-root
   #:specialized-environment
   #:specialized-type-definitions
   #:specialized-origin
   #:monomorphize
   #:defunctionalize
   #:check-defunctionalized
   #:recursive-types
   #:check-monomorphic
   #:lower
   #:lowered
   #:lowered-defs
   #:lowered-types
   #:lowered-root
   #:lowered-constructors
   #:check-lowered
   #:run-lowered
   #:walk-ir
   #:assign-stacks
   #:check-assigned
   #:check-roots
   #:root-summary #:root-summary-result #:root-summary-stores #:root-summary-locals
   #:emit-c
   #:write-c
   #:size-class
   #:lowered-stack-count
   #:ir-type #:ir-type-kind #:ir-type-name #:ir-type-args
   #:ir-constructor #:ir-constructor-name #:ir-constructor-tag #:ir-constructor-fields
   #:ir-def #:ir-def-name #:ir-def-origin #:ir-def-kind #:ir-def-params
   #:ir-def-result #:ir-def-body #:ir-def-location
   #:ir-def-source #:ir-def-destination #:ir-def-pool
   #:ir-node #:ir-node-type #:ir-node-location
   #:ir-atom
   #:ir-literal #:ir-literal-value
   #:ir-local #:ir-local-name
   #:ir-global #:ir-global-name
   #:ir-let #:ir-let-name #:ir-let-value #:ir-let-body #:ir-let-stack
   #:ir-call #:ir-call-function #:ir-call-args #:ir-call-destination #:ir-call-region
   #:ir-construct #:ir-construct-constructor #:ir-construct-args #:ir-construct-stack
   #:ir-prim #:ir-prim-op #:ir-prim-args #:ir-prim-stack
   #:ir-foreign #:ir-foreign-vars #:ir-foreign-form
   #:ir-match #:ir-match-scrutinee #:ir-match-branches
   #:ir-branch #:ir-branch-pattern #:ir-branch-body
   #:ir-pattern #:ir-pattern-type
   #:ir-pvar #:ir-pvar-name
   #:ir-pwild
   #:ir-plit #:ir-plit-value
   #:ir-pas #:ir-pas-name #:ir-pas-pattern
   #:ir-pcon #:ir-pcon-constructor #:ir-pcon-patterns))

(defpackage #:sheaf/generated
  (:use))
