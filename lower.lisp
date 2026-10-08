;;;; lower.lisp
;;;; Lowering a defunctionalized program to Sheaf's own first-order IR in
;;;; A-normal form.

(in-package #:sheaf)

;;; Types

(defstruct (ir-type (:constructor make-ir-type (key kind name args)))
  "A ground type. Equal types are the same object."
  (key nil :read-only t)
  (kind :scalar :type (member :scalar :data :reference) :read-only t)
  (name nil :type symbol :read-only t)
  (args '() :type list :read-only t))

(defmethod print-object ((type ir-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~{~a~^ ~}" (cons (ir-type-name type) (ir-type-args type)))))

(defstruct (ir-constructor (:constructor make-ir-constructor (name tag fields)))
  "A constructor of a data type, its position in the type and its field types."
  (name nil :type symbol :read-only t)
  (tag 0 :type fixnum :read-only t)
  (fields '() :type list :read-only t))

;;; Nodes

(defstruct (ir-node (:constructor nil))
  (type nil :type (or null ir-type))
  (location nil :type (or null source:location)))

(defstruct (ir-literal (:include ir-node)) value)
(defstruct (ir-local (:include ir-node)) name)
(defstruct (ir-global (:include ir-node)) name)
(defstruct (ir-let (:include ir-node)) name value body (stack nil))
(defstruct (ir-call (:include ir-node)) function args (destination nil) (region nil))
(defstruct (ir-construct (:include ir-node)) constructor args (stack nil))
(defstruct (ir-prim (:include ir-node)) op args)
(defstruct (ir-foreign (:include ir-node)) vars form)
(defstruct (ir-match (:include ir-node)) scrutinee branches)
(defstruct ir-branch pattern body)

(defstruct (ir-pattern (:constructor nil))
  (type nil :type (or null ir-type)))
(defstruct (ir-pvar (:include ir-pattern)) name)
(defstruct (ir-pwild (:include ir-pattern)))
(defstruct (ir-plit (:include ir-pattern)) value)
(defstruct (ir-pas (:include ir-pattern)) name pattern)
(defstruct (ir-pcon (:include ir-pattern)) constructor patterns)

(deftype ir-atom () '(or ir-literal ir-local ir-global))

;;; Programs

(defstruct (ir-def (:constructor make-ir-def (name origin kind params result body location)))
  "A global. A `:function` has parameters, each a name and a type; a `:value`
has none. Stack assignment clones a def per context and fills the last three
slots."
  name origin kind params result body location
  (source nil) (destination nil) (pool nil))

(defstruct (lowered (:constructor %make-lowered))
  "A program in Sheaf's IR: its globals, the constructors of every data type it
reaches, and its root."
  (defs (make-hash-table :test 'eq) :type hash-table :read-only t)
  (types (make-hash-table :test 'eq) :type hash-table :read-only t)
  (root nil :type symbol)
  (stack-count nil :type (or null fixnum)))

(defun lowered-constructors (lowered type)
  "The constructors of the data type TYPE, in tag order."
  (values (gethash type (lowered-types lowered))))

(defun walk-ir (function node)
  "Call FUNCTION on NODE and on every node inside it, outermost first."
  (funcall function node)
  (etypecase node
    ((or ir-literal ir-local ir-global ir-foreign))
    (ir-let (walk-ir function (ir-let-value node))
            (walk-ir function (ir-let-body node)))
    ((or ir-call ir-construct ir-prim)
     (dolist (arg (etypecase node
                    (ir-call (ir-call-args node))
                    (ir-construct (ir-construct-args node))
                    (ir-prim (ir-prim-args node))))
       (walk-ir function arg)))
    (ir-match (walk-ir function (ir-match-scrutinee node))
              (dolist (branch (ir-match-branches node))
                (walk-ir function (ir-branch-body branch))))))

;;; Primitives

(defconstant +ufix-modulus+ (expt 2 63))

;; TODO: only the UFix operations and Ordering equality that the fixtures reach
;; are primitive, and they are matched by the name of the instance method.
;; Cover the other numeric types and the rest of Num, Eq and Ord.
;; https://todo.sr.ht/~takeiteasy/sheaf/23
(defparameter *primitives*
  (let ((table (make-hash-table :test 'equal)))
    (loop :for (name op arity)
            :in '(("INSTANCE/CLS:NUM UFIX-COALTON/CLASSES:+" :ufix-add 2)
                  ("INSTANCE/CLS:NUM UFIX-COALTON/CLASSES:-" :ufix-sub 2)
                  ("INSTANCE/CLS:NUM UFIX-COALTON/CLASSES:*" :ufix-mul 2)
                  ("INSTANCE/CLS:NUM UFIX-COALTON/CLASSES:FROMINT" :ufix-from-int 1)
                  ("INSTANCE/CLS:EQ UFIX-COALTON/CLASSES:==" :ufix-eq 2)
                  ("INSTANCE/CLS:ORD UFIX-COALTON/CLASSES:<=>" :ufix-compare 2)
                  ("INSTANCE/EQ ORDERING-COALTON/CLASSES:==" :ordering-eq 2))
          :do (setf (gethash name table) (list op arity)))
    table)
  "Instance methods that lower to a `prim`, by name: the operation and its arity.")

(defvar *specialized*)

(defun primitive-of (name)
  "The operation and arity NAME is a primitive of, or NIL."
  (let ((origin (or (specialized-origin *specialized* name) name)))
    (values-list (gethash (symbol-name origin) *primitives*))))

(defun primitive-arity (op)
  (loop :for (entry-op arity) :being :the :hash-values :of *primitives*
        :when (eq op entry-op) :return arity))

;;; Lowering

(defvar *def* nil "The name and type of the global being lowered, for errors.")
(defvar *location* nil)
(defvar *bindings*)
(defvar *ir-types*)
(defvar *pending-types*)

(defun lowering-error (control &rest args)
  (error 'specialization-error
         :message (format nil "Lowering does not support ~?" control args)
         :chain (and *def* (list (list (car *def*) (cdr *def*) *location*)))))

(defparameter *opaque-types*
  '(coalton/cell:cell coalton/vector:vector coalton/hashtable:hashtable)
  "Types that are references to mutable storage, whose meaning is open (#8).")

(defun lower-type (type)
  (etypecase type
    ((or tc:tycon tc:tapp)
     (let ((head (first (tc:flatten-type type)))
           (key (type-key type)))
       (unless (tc:tycon-p head)
         (lowering-error "the type ~a" (tc:type-to-string type)))
       (or (gethash key *ir-types*)
           (let* ((name (tc:tycon-name head))
                  (entry (tc:lookup-type entry:*global-environment* name :no-error t))
                  (kind (cond ((member name *opaque-types*) :reference)
                              ((and entry (or (tc:type-entry-constructors entry)
                                              (eq (symbol-package name) (find-package '#:sheaf/generated))))
                               :data)
                              (t :scalar)))
                  (ir-type (make-ir-type key kind name (mapcar #'lower-type (rest (tc:flatten-type type))))))
             (setf (gethash key *ir-types*) ir-type)
             (when (eq kind :data)
               (push (cons ir-type type) *pending-types*))
             ir-type))))
    ((or tc:tyvar tc:function-ty tc:result-ty)
     (lowering-error "the type ~a" (tc:type-to-string type)))))

(defun instance-fields (constructor type)
  "The field types of CONSTRUCTOR of the ground type TYPE."
  (let* ((scheme (tc:lookup-value-type entry:*global-environment* constructor))
         (constructor-type (tc:qualified-ty-type (tc:fresh-inst scheme)))
         (functionp (tc:function-type-p constructor-type))
         (result (if functionp (tc:function-return-type constructor-type) constructor-type))
         (substitution (loop :for var :in (rest (tc:flatten-type result))
                             :for arg :in (rest (tc:flatten-type type))
                             :collect (tc:make-substitution :from var :to arg))))
    (when functionp
      (mapcar (lambda (field) (tc:apply-substitution substitution field))
              (tc:function-type-arguments constructor-type)))))

(defun build-type-table (lowered)
  (loop :while *pending-types*
        :do (destructuring-bind (ir-type . type) (pop *pending-types*)
              (let ((entry (tc:lookup-type entry:*global-environment* (ir-type-name ir-type))))
                (setf (gethash ir-type (lowered-types lowered))
                      (loop :for constructor :in (tc:type-entry-constructors entry)
                            :for tag :from 0
                            :collect (make-ir-constructor
                                      constructor tag
                                      (mapcar #'lower-type (instance-fields constructor type)))))))))

(defun emit (name value)
  (push (list name value) *bindings*))

(defun wrap-bindings (tail)
  "TAIL inside a let for each binding emitted so far."
  (let ((body tail))
    (loop :for (name value) :in *bindings*
          :do (setf body (make-ir-let :name name :value value :body body
                                      :type (ir-node-type tail)
                                      :location (ir-node-location value))))
    body))

(defun lower-body (node)
  (let ((*bindings* '()))
    (let ((tail (lower-simple node)))
      (wrap-bindings tail))))

(defun to-atom (node)
  (let ((simple (lower-simple node)))
    (if (typep simple 'ir-atom)
        simple
        (let ((name (make-symbol "T")))
          (emit name simple)
          (make-ir-local :name name :type (ir-node-type simple) :location (ir-node-location simple))))))

(defun lowered-global-p (name)
  (nth-value 1 (gethash name (specialized-bindings *specialized*))))

(defun lower-variable (node)
  (let ((name (ast:node-variable-value node))
        (type (lower-type (ast:node-type node))))
    (cond ((constructor-p name)
           (lower-call name '() (ast:node-type node)))
          ((lowered-global-p name)
           (make-ir-global :name name :type type :location *location*))
          ((typep node 'ast:node-local-variable)
           (make-ir-local :name name :type type :location *location*))
          (t (lowering-error "the reference to ~s, which is not specialized" name)))))

(defun lower-call (name rands type)
  (let ((args (mapcar #'to-atom rands))
        (type (lower-type type)))
    (multiple-value-bind (op arity) (primitive-of name)
      (cond ((constructor-p name)
             (make-ir-construct :constructor name :args args :type type :location *location*))
            (op
             (unless (= arity (length args))
               (lowering-error "a call of ~s with ~d arguments" name (length args)))
             (if (and (eq op :ufix-from-int) (typep (first args) 'ir-literal))
                 (make-ir-literal :value (mod (ir-literal-value (first args)) +ufix-modulus+)
                                  :type type :location *location*)
                 (make-ir-prim :op op :args args :type type :location *location*)))
            ((lowered-global-p name)
             (make-ir-call :function name :args args :type type :location *location*))
            (t (lowering-error "a call of ~s, which is not specialized" name))))))

(defun callee-name (name)
  (unless (or (lowered-global-p name) (constructor-p name) (primitive-of name))
    (lowering-error "a call through the function value ~s" name))
  name)

(defun lower-let (node)
  (loop :for ((name . value) . later) :on (ast:node-let-bindings node)
        :do (when (intersection (cons name (mapcar #'car later)) (referenced-names value))
              (lowering-error "the recursive binding ~s" name))
            (emit name (lower-simple value)))
  (lower-simple (ast:node-let-subexpr node)))

(defun lower-pattern (pattern)
  (let ((type (lower-type (pattern:pattern-type pattern))))
    (etypecase pattern
      (pattern:pattern-var (make-ir-pvar :name (pattern:pattern-var-name pattern) :type type))
      (pattern:pattern-wildcard (make-ir-pwild :type type))
      (pattern:pattern-literal (make-ir-plit :value (pattern:pattern-literal-value pattern) :type type))
      (pattern:pattern-binding
       (make-ir-pas :name (pattern:pattern-var-name (pattern:pattern-binding-var pattern))
                    :pattern (lower-pattern (pattern:pattern-binding-pattern pattern))
                    :type type))
      (pattern:pattern-constructor
       (make-ir-pcon :constructor (pattern:pattern-constructor-name pattern)
                     :patterns (mapcar #'lower-pattern (pattern:pattern-constructor-patterns pattern))
                     :type type)))))

(defun lower-match (node)
  (let ((scrutinee (to-atom (ast:node-match-expr node))))
    (make-ir-match :scrutinee scrutinee :type (lower-type (ast:node-type node)) :location *location*
                   :branches (loop :for branch :in (ast:node-match-branches node)
                                   :collect (make-ir-branch
                                             :pattern (lower-pattern (ast:match-branch-pattern branch))
                                             :body (lower-body (ast:match-branch-body branch)))))))

(defun lower-simple (node)
  "The IR of NODE as an atom or one simple expression, after emitting a binding
for everything it evaluates first."
  (let ((*location* (or (ast:node-location node) *location*)))
    (etypecase node
      (ast:node-literal
       (make-ir-literal :value (ast:node-literal-value node)
                        :type (lower-type (ast:node-type node)) :location *location*))
      (ast:node-variable (lower-variable node))
      (ast:node-application
       (when (ast:node-application-keyword-rands node)
         (lowering-error "keyword arguments"))
       (unless (typep (ast:node-application-rator node) 'ast:node-variable)
         (lowering-error "a call through a function value"))
       (lower-call (callee-name (ast:node-variable-value (ast:node-application-rator node)))
                   (ast:node-application-rands node) (ast:node-type node)))
      (ast:node-direct-application
       (when (ast:node-direct-application-keyword-rands node)
         (lowering-error "keyword arguments"))
       (lower-call (callee-name (ast:node-direct-application-rator node))
                   (ast:node-direct-application-rands node) (ast:node-type node)))
      (ast:node-let (lower-let node))
      (ast:node-bind
       (emit (ast:node-bind-name node) (lower-simple (ast:node-bind-expr node)))
       (lower-simple (ast:node-bind-body node)))
      (ast:node-seq
       (loop :for (child . later) :on (ast:node-seq-nodes node)
             :if later :do (emit (make-symbol "_") (lower-simple child))
               :else :return (lower-simple child)))
      (ast:node-locally (lower-simple (ast:node-locally-subexpr node)))
      (ast:node-match (lower-match node))
      (ast:node-lisp
       (make-ir-foreign :vars (ast:node-lisp-vars node) :form (ast:node-lisp-form node)
                        :type (lower-type (ast:node-type node)) :location *location*))
      (ast:node-abstraction (lowering-error "a lambda below the top level")))))

(defun lower-def (name node)
  (let* ((*def* (cons name (ast:node-type node)))
         (*location* (ast:node-location node))
         (origin (specialized-origin *specialized* name)))
    (if (typep node 'ast:node-abstraction)
        (let ((type (ast:node-type node)))
          (when (ast:node-abstraction-keyword-params node)
            (lowering-error "keyword arguments"))
          (make-ir-def name origin :function
                       (loop :for var :in (ast:node-abstraction-vars node)
                             :for param :in (tc:function-type-arguments type)
                             :collect (cons var (lower-type param)))
                       (lower-type (tc:function-return-type type))
                       (lower-body (ast:node-abstraction-subexpr node))
                       *location*))
        (make-ir-def name origin :value '()
                     (lower-type (ast:node-type node))
                     (lower-body node)
                     *location*))))

(defun lower (specialized)
  "SPECIALIZED, a defunctionalized program, in Sheaf's IR."
  (check-defunctionalized specialized)
  (let* ((entry:*global-environment* (specialized-environment specialized))
         (*specialized* specialized)
         (*ir-types* (make-hash-table :test 'equal))
         (*pending-types* '())
         (lowered (%make-lowered :root (specialized-root specialized))))
    (maphash (lambda (name node)
               (unless (primitive-of name)
                 (setf (gethash name (lowered-defs lowered)) (lower-def name node))))
             (specialized-bindings specialized))
    (build-type-table lowered)
    lowered))

;;; Checking

(defun check-lowered (lowered)
  "Signal an error unless every name in LOWERED is bound, every call, constructor
and primitive has its arity, and every operand is an atom. Returns T."
  (maphash
   (lambda (name def)
     (labels ((fail (control &rest args)
                (error "~s: ~?" name control args))
              (check-atom (node scope)
                (typecase node
                  (ir-literal)
                  (ir-local (unless (member (ir-local-name node) scope)
                              (fail "~s is not bound" (ir-local-name node))))
                  (ir-global (let ((def (gethash (ir-global-name node) (lowered-defs lowered))))
                               (unless (and def (eq :value (ir-def-kind def)))
                                 (fail "~s is not a global value" (ir-global-name node)))))
                  (t (fail "~a is an operand, not an atom" (type-of node)))))
              (check-constructor (constructor type arity)
                (let ((entry (find constructor (lowered-constructors lowered type)
                                   :key #'ir-constructor-name)))
                  (unless entry
                    (fail "~s is not a constructor of ~a" constructor type))
                  (unless (= arity (length (ir-constructor-fields entry)))
                    (fail "~s takes ~d fields, not ~d"
                          constructor (length (ir-constructor-fields entry)) arity))))
              (check-pattern (pattern)
                "The variables PATTERN binds."
                (etypecase pattern
                  (ir-pwild '())
                  (ir-plit '())
                  (ir-pvar (list (ir-pvar-name pattern)))
                  (ir-pas (cons (ir-pas-name pattern) (check-pattern (ir-pas-pattern pattern))))
                  (ir-pcon
                   (check-constructor (ir-pcon-constructor pattern) (ir-pattern-type pattern)
                                      (length (ir-pcon-patterns pattern)))
                   (mapcan #'check-pattern (ir-pcon-patterns pattern)))))
              (check (node scope)
                (unless (ir-node-type node)
                  (fail "a ~a has no type" (type-of node)))
                (etypecase node
                  (ir-atom (check-atom node scope))
                  (ir-let
                   (when (typep (ir-let-value node) 'ir-let)
                     (fail "the value of ~s is a let" (ir-let-name node)))
                   (check (ir-let-value node) scope)
                   (check (ir-let-body node) (cons (ir-let-name node) scope)))
                  (ir-call
                   (let ((callee (gethash (ir-call-function node) (lowered-defs lowered))))
                     (unless (and callee (eq :function (ir-def-kind callee)))
                       (fail "~s is not a global function" (ir-call-function node)))
                     (unless (= (length (ir-def-params callee)) (length (ir-call-args node)))
                       (fail "~s takes ~d arguments, not ~d" (ir-call-function node)
                             (length (ir-def-params callee)) (length (ir-call-args node)))))
                   (dolist (arg (ir-call-args node)) (check-atom arg scope)))
                  (ir-construct
                   (check-constructor (ir-construct-constructor node) (ir-node-type node)
                                      (length (ir-construct-args node)))
                   (dolist (arg (ir-construct-args node)) (check-atom arg scope)))
                  (ir-prim
                   (unless (eql (primitive-arity (ir-prim-op node)) (length (ir-prim-args node)))
                     (fail "~s has the wrong number of arguments" (ir-prim-op node)))
                   (dolist (arg (ir-prim-args node)) (check-atom arg scope)))
                  (ir-foreign
                   (loop :for (nil . var) :in (ir-foreign-vars node)
                         :unless (member var scope)
                           :do (fail "~s is not bound" var)))
                  (ir-match
                   (check-atom (ir-match-scrutinee node) scope)
                   (dolist (branch (ir-match-branches node))
                     (check (ir-branch-body branch)
                            (append (check-pattern (ir-branch-pattern branch)) scope)))))))
       (check (ir-def-body def) (mapcar #'car (ir-def-params def)))))
   (lowered-defs lowered))
  t)
