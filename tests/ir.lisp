;;;; ir.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defvar *counter* nil)

(defun counter ()
  (or *counter*
      (setf *counter*
            (sheaf:compile-program
             (asdf:system-relative-pathname "sheaf" "tests/fixtures/counter.lisp")))))

(defun fixture (name)
  (find-symbol name "SHEAF/FIXTURES/COUNTER"))

(defun count-lisp-nodes (node)
  (let ((n 0))
    (traverse:traverse
     node
     (list (traverse:action (:after ast:node-lisp _) (incf n) (values))))
    n))

(test captures-each-definition
  (dolist (name '("SUM-SQUARES" "MAKE-COUNTER" "MAIN"))
    (multiple-value-bind (node captured) (sheaf:binding-code (counter) (fixture name))
      (is-true captured)
      (is (typep node 'ast:node-abstraction)))))

(test captured-bindings-have-locations
  (maphash (lambda (name node)
             (is-true (ast:node-location node) "~s has no location" name))
           (sheaf:program-bindings (counter))))

(test captured-ir-has-no-lisp-escapes
  (maphash (lambda (name node)
             (is (zerop (count-lisp-nodes node)) "~s holds raw Lisp" name))
           (sheaf:program-bindings (counter))))

(test reachable-from-main
  (multiple-value-bind (with-code without-code)
      (sheaf:reachable (counter) (fixture "MAIN"))
    (dolist (name (list (fixture "MAKE-COUNTER")
                        (fixture "SUM-SQUARES")
                        'coalton/cell:new
                        'coalton/iterator:fold!))
      (is (member name with-code) "~s is not reached" name))
    (is (member 'coalton/classes:+ without-code))))

(test captured-program-still-runs
  (is (= 287 (funcall (fixture "MAIN")))))
