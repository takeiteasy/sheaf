;;;; sheaf.asd

(asdf:defsystem #:sheaf
  :description "A statically typed Lisp with no heap, built on a Coalton frontend"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :version "0.0.1"
  :depends-on (#:coalton)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "ir"))
  :in-order-to ((asdf:test-op (asdf:test-op #:sheaf/tests))))

(asdf:defsystem #:sheaf/tests
  :depends-on (#:sheaf #:fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "ir"))
  :perform (asdf:test-op (o c) (uiop:symbol-call '#:sheaf/tests '#:run)))
