;;;; package.lisp

(defpackage #:sheaf/tests
  (:use #:cl #:fiveam)
  (:shadow #:run)
  (:local-nicknames
   (#:ast #:coalton-impl/codegen/ast)
   (#:traverse #:coalton-impl/codegen/traverse))
  (:export #:run))

(in-package #:sheaf/tests)

(def-suite sheaf)

(defun run ()
  (run! 'sheaf))
