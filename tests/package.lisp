;;;; package.lisp

(defpackage #:sheaf/tests
  (:use #:cl #:fiveam)
  (:shadow #:run)
  (:local-nicknames
   (#:ast #:coalton-impl/codegen/ast)
   (#:entry #:coalton-impl/entry)
   (#:settings #:coalton-impl/settings)
   (#:tc #:coalton-impl/typechecker)
   (#:traverse #:coalton-impl/codegen/traverse))
  (:export #:run))

(in-package #:sheaf/tests)

(def-suite sheaf)

(defun run ()
  (run! 'sheaf))

(defvar *programs* (make-hash-table :test 'equal))

(defun fixture-program (fixture)
  "The program of tests/fixtures/FIXTURE.lisp, compiled once."
  (or (gethash fixture *programs*)
      (setf (gethash fixture *programs*)
            (sheaf:compile-program
             (asdf:system-relative-pathname "sheaf" (format nil "tests/fixtures/~a.lisp" fixture))))))

(defun fixture-symbol (fixture name)
  (find-symbol name (format nil "SHEAF/FIXTURES/~:@(~a~)" fixture)))
