;;;; monomorphize.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defvar *specialized* (make-hash-table :test 'equal))

(defun specialized (fixture)
  "FIXTURE's program monomorphized from its main, once."
  (or (gethash fixture *specialized*)
      (setf (gethash fixture *specialized*)
            (sheaf:monomorphize (fixture-program fixture) (fixture-symbol fixture "MAIN")))))

(defun specializations-of (specialized name)
  "The erased types NAME is specialized at, as strings."
  (loop :for symbol :being :the :hash-keys :of (sheaf:specialized-bindings specialized)
          :using (:hash-value node)
        :when (eq name (sheaf:specialized-origin specialized symbol))
          :collect (tc:type-to-string (ast:node-type node))))

(defun run-specialized (specialized)
  "Compile SPECIALIZED to Lisp with Coalton's backend and call its root."
  ;; Raw Lisp from the stdlib, such as an assert, returns NIL where its type
  ;; says Unit, and Coalton's type annotations reject that.
  (let* ((env (sheaf:specialized-environment specialized))
         (entry:*global-environment* env)
         (bindings (loop :for name :being :the :hash-keys :of (sheaf:specialized-bindings specialized)
                           :using (:hash-value node)
                         :collect (cons name node)))
         (types (loop :for definition :in (sheaf:specialized-type-definitions specialized)
                      :append (codegen-type-definition:codegen-type-definition definition env)))
         (forms (let ((settings:*emit-type-annotations* nil))
                  (loop :for scc :in (ast:node-binding-sccs bindings)
                        :append (coalton-impl/codegen/program::compile-scc
                                 (remove-if-not (lambda (binding) (find (car binding) scc)) bindings)
                                 env))))
         (thunk (progn
                  (mapc #'eval types)
                  (let ((*error-output* (make-broadcast-stream)))
                    (compile nil `(lambda ()
                                    ,@forms
                                    (funcall ,(sheaf:specialized-root specialized))))))))
    (funcall thunk)))

(test specializations-are-monomorphic
  (dolist (fixture '("counter" "poly"))
    (is-true (sheaf:check-monomorphic (specialized fixture)))))

(test specialized-programs-run
  (is (= 287 (run-specialized (specialized "counter"))))
  (is (= 19 (run-specialized (specialized "poly")))))

(test generic-specialized-per-type
  (is (equal '("F32 → F32" "UFIX → UFIX")
             (sort (specializations-of (specialized "poly") (fixture-symbol "poly" "TWICE"))
                   #'string<))))

(test constrained-instance-method-resolved
  (is (equal '("OPTIONAL UFIX * OPTIONAL UFIX → BOOLEAN")
             (specializations-of (specialized "poly")
                                 (find-symbol "INSTANCE/EQ (OPTIONAL :A)-COALTON/CLASSES:=="
                                              "COALTON/OPTIONAL")))))

(test local-binding-cloned-per-type
  (let ((main (gethash (sheaf:specialized-root (specialized "poly"))
                       (sheaf:specialized-bindings (specialized "poly"))))
        (clones '()))
    (traverse:traverse
     main
     (list (traverse:action (:after ast:node-let n)
             (loop :for (name . value) :in (ast:node-let-bindings n)
                   :when (search "DBL" (symbol-name name))
                     :do (push (tc:type-to-string (ast:node-type value)) clones))
             (values))))
    (is (equal '("F32 → F32" "UFIX → UFIX") (sort clones #'string<)))))

(test open-type-defaults-to-unit
  (is (equal '("LIST UNIT → UFIX")
             (specializations-of (specialized "poly") 'coalton/list:length))))

(test specializations-keep-locations
  (maphash (lambda (name node)
             (is-true (ast:node-location node) "~s has no location" name)
             (traverse:traverse
              node
              (list (traverse:action (:after ast:node n)
                      (is-true (ast:node-location n) "A ~s in ~s has no location" (type-of n) name)
                      (values)))))
           (sheaf:specialized-bindings (specialized "poly"))))

(test polymorphic-recursion-is-bounded
  (let* ((program (fixture-program "polyrec"))
         (deepen (fixture-symbol "polyrec" "DEEPEN"))
         (chain (handler-case
                    (progn (sheaf:monomorphize program (fixture-symbol "polyrec" "MAIN"))
                           nil)
                  (sheaf:specialization-error (e) (sheaf:specialization-error-chain e)))))
    (is (= (+ 2 sheaf:*specialization-limit*) (length chain)))
    (is (every (lambda (link) (eq deepen (first link))) (rest chain)))))

(test a-loop-variable-of-generic-code-has-a-ground-type
  (let ((bindings 0))
    (maphash (lambda (name node)
               (declare (ignore name))
               (traverse:traverse
                node
                (list (traverse:action (:after ast:node-for loop)
                        (dolist (binding (ast:node-for-bindings loop))
                          (incf bindings)
                          (is (null (tc:type-variables (ast:node-for-binding-type binding)))))
                        loop))))
             (sheaf:specialized-bindings (specialized "generic-loops")))
    (is (= 4 bindings))
    (is (= 5 (run-on-sbcl "generic-loops")))))
