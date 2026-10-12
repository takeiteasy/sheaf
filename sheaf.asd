;;;; sheaf.asd

(asdf:defsystem #:sheaf/prelude
  :description "Array and reference types for Sheaf programs"
  :license "GPL-3.0-or-later"
  :depends-on (#:coalton)
  :pathname "prelude/"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "fixed")
               (:file "arr")
               (:file "grow")
               (:file "stack")
               (:file "ref")
               (:file "vector")
               (:file "hashtable")))

(asdf:defsystem #:sheaf
  :description "A statically typed Lisp with no heap, built on a Coalton frontend"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :version "0.0.1"
  :depends-on (#:coalton #:sheaf/prelude)
  :serial t
  :components ((:file "package")
               (:file "ir")
               (:file "numbers")
               (:file "monomorphize")
               (:file "prelude-ops")
               (:file "defunctionalize")
               (:file "lower")
               (:file "stacks")
               (:file "roots")
               (:file "interpret")
               (:file "c-ast")
               (:file "emit"))
  :in-order-to ((asdf:test-op (asdf:test-op #:sheaf/tests))))

(asdf:defsystem #:sheaf/tests
  :depends-on (#:sheaf #:fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "ir")
               (:file "monomorphize")
               (:file "defunctionalize")
               (:file "lower")
               (:file "stacks")
               (:file "c-ast")
               (:file "emit")
               (:file "prelude")
               (:file "numbers")
               (:file "roots")
               (:file "exceptions"))
  :perform (asdf:test-op (o c) (uiop:symbol-call '#:sheaf/tests '#:run)))
