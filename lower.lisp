;;;; lower.lisp
;;;; Lowering a defunctionalized program to Sheaf's own first-order IR in
;;;; A-normal form.

(in-package #:sheaf)

;;; Types

(defstruct (ir-type (:constructor make-ir-type (key kind name args)))
  "A ground type. Equal types are the same object."
  (key nil :read-only t)
  (kind :scalar :type (member :scalar :data :storage :view) :read-only t)
  (name nil :type symbol :read-only t)
  (args '() :type list :read-only t))

(defmethod print-object ((type ir-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~{~a~^ ~}" (cons (ir-type-name type) (ir-type-args type)))))

(defun type-text (type)
  "TYPE as text, for messages."
  (format nil "~a~{ ~a~}" (ir-type-name type)
          (mapcar (lambda (arg)
                    (if (ir-type-args arg) (format nil "(~a)" (type-text arg)) (type-text arg)))
                  (ir-type-args type))))

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
(defstruct (ir-let (:include ir-node)) name value body (stacks '()) (bank nil))
(defstruct (ir-call (:include ir-node)) function args (destination nil) (growables '()) (bank nil) (region nil) (spare nil) (copy nil) (reset nil))
(defstruct (ir-construct (:include ir-node)) constructor args (stack nil))
(defstruct (ir-prim (:include ir-node)) op args (stack nil) (bank nil))
(defstruct (ir-foreign (:include ir-node)) vars form)
(defstruct (ir-panic (:include ir-node)) message)  ; a `String` atom
(defstruct (ir-match (:include ir-node)) scrutinee branches)
(defstruct ir-branch pattern body)
(defstruct (ir-block (:include ir-node)) label body)
(defstruct (ir-return (:include ir-node)) label value)
(defstruct (ir-loop (:include ir-node)) label vars inits sequential test exit-on steps body returns (reset '()))
(defstruct (ir-break (:include ir-node)) label)
(defstruct (ir-continue (:include ir-node)) label)
(defstruct (ir-throw (:include ir-node)) value)
(defstruct (ir-catch (:include ir-node)) body branches (busy '()) (marks '()) (bank-marks '()))
(defstruct ir-catch-branch type pattern body)
(defstruct (ir-protect (:include ir-node)) body cleanup (stacks '()) (bank nil) (busy '()) (marks '()) (bank-marks '()))

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
has none. Stack assignment clones a def per context and fills the last five
slots."
  name origin kind params result body location
  (source nil) (destination nil) (growables '()) (bank nil) (pool nil))

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
    ((or ir-literal ir-local ir-global ir-foreign ir-break ir-continue))
    (ir-panic (walk-ir function (ir-panic-message node)))
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
                (walk-ir function (ir-branch-body branch))))
    (ir-block (walk-ir function (ir-block-body node)))
    (ir-return (walk-ir function (ir-return-value node)))
    (ir-loop (mapc (lambda (init) (walk-ir function init)) (ir-loop-inits node))
             (when (ir-loop-test node) (walk-ir function (ir-loop-test node)))
             (dolist (step (ir-loop-steps node))
               (when step (walk-ir function step)))
             (walk-ir function (ir-loop-body node))
             (walk-ir function (ir-loop-returns node)))
    (ir-throw (walk-ir function (ir-throw-value node)))
    (ir-catch (walk-ir function (ir-catch-body node))
              (dolist (branch (ir-catch-branches node))
                (walk-ir function (ir-catch-branch-body branch))))
    (ir-protect (walk-ir function (ir-protect-body node))
                (walk-ir function (ir-protect-cleanup node)))))

;;; Primitives

;; Num, Eq and Ord methods of the numeric types, and Eq of Ordering, are
;; primitives. The method of each class in the order it declares them:
(defparameter *numeric-classes*
  '((coalton/classes:num :add :sub :mul :from-int)
    (coalton/classes:eq :eq)
    (coalton/classes:ord :compare)))

(defparameter *numeric-ops*
  '((:add 2) (:sub 2) (:mul 2) (:from-int 1) (:eq 2) (:compare 2))
  "The arity of each operation on numbers.")

(defvar *numeric-primitives* (make-hash-table :test 'eq)
  "The operation of each numeric instance method by origin, for the program being lowered.")

(defun numeric-primitives ()
  (let ((table (make-hash-table :test 'eq)))
    (loop :for (class . ops) :in *numeric-classes*
          :do (dolist (instance (tc:lookup-class-instances entry:*global-environment* class :no-error t))
                (let ((type (first (tc:ty-predicate-types (tc:ty-class-instance-predicate instance)))))
                  (when (and (tc:tycon-p type)
                             (or (numeric-entry (tc:tycon-name type))
                                 (and (eq class 'coalton/classes:eq)
                                      (eq (tc:tycon-name type) 'coalton/classes:ordering))))
                    (loop :for symbol :in (tc:ty-class-instance-method-codegen-syms instance)
                          :for op :in ops
                          :do (setf (gethash symbol table) op))))))
    table))

(defvar *specialized*)

(defvar *return-targets* '()
  "The blocks being lowered, innermost first: a name, its label and the handler depth it is at.")

(defvar *loop-labels* '()
  "The loop bodies being lowered, innermost first: a name, its label and the handler depth it is at.")

(defvar *renames* '()
  "The names of the loop variables a later initial value of a `for*` refers to, each with the local that holds its initial value.")

(defvar *exit-labels* '()
  "The labels of the blocks the node being checked is inside.")

(defvar *handler-depth* 0
  "How many catches and protects the node being lowered is inside.")

(defun primitive-of (name)
  "The operation and arity NAME is a primitive of, or NIL."
  (let* ((origin (or (specialized-origin *specialized* name) name))
         (op (gethash origin *prelude-primitives*)))
    (if op
        (values op (second (assoc op *prelude-ops*)))
        (let ((op (gethash origin *numeric-primitives*)))
          (and op (values op (second (assoc op *numeric-ops*))))))))

(defun primitive-arity (op)
  (second (or (assoc op *prelude-ops*) (assoc op *numeric-ops*))))

(defun prelude-op-p (op)
  (and (assoc op *prelude-ops*) t))

(defun primitive-allocates (op)
  "Whether the primitive OP allocates storage."
  (and (third (assoc op *prelude-ops*)) t))

;;; Lowering

(defvar *def* nil "The name and type of the global being lowered, for errors.")
(defvar *location* nil)
(defvar *bindings*)
(defvar *ir-types*)
(defvar *pending-types*)
(defvar *exception-sites* '() "Each exception type thrown or caught, the def and the location, for the payload check.")

(defun lowering-error (control &rest args)
  (error 'specialization-error
         :message (format nil "Lowering does not support ~?" control args)
         :chain (and *def* (list (list (car *def*) (cdr *def*) *location*)))))

(defun fixed-length (type)
  "The length of the fixed array TYPE."
  (sheaf/prelude:fixed-length (ir-type-name (first (ir-type-args type)))))

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
                  (kind (cond ((assoc name *prelude-types*) (second (assoc name *prelude-types*)))
                              ((and entry (or (tc:type-entry-constructors entry)
                                              (eq (symbol-package name) (find-package '#:sheaf/generated))))
                               :data)
                              (t :scalar)))
                  (ir-type (make-ir-type key kind name (mapcar #'lower-type (rest (tc:flatten-type type))))))
             (when (and (eq name 'sheaf/prelude:fixedarray) (not (fixed-length ir-type)))
               (lowering-error "the length type of ~a, which define-length does not declare"
                               (tc:type-to-string type)))
             (when (and (eq name 'coalton/hashtable:hashtable)
                        (not (eq (ir-type-name (first (ir-type-args ir-type))) 'coalton:ufix)))
               (lowering-error "the hashtable key type ~a: https://github.com/takeiteasy/sheaf/issues/20"
                               (tc:type-to-string type)))
             (setf (gethash key *ir-types*) ir-type)
             (when (eq kind :data)
               (push (cons ir-type type) *pending-types*))
             ir-type))))
    (tc:result-ty
     (if (tc:result-ty-output-types type)
         (lowering-error "the type ~a" (tc:type-to-string type))
         (lower-type tc:*unit-type*)))
    ((or tc:tyvar tc:function-ty)
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

(defvar *local-types* nil "The type of each local of the def being lowered, by name.")

(defun emit (name value)
  (when *local-types*
    (setf (gethash name *local-types*) (ir-node-type value)))
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
    (cond ((or (constructor-p name) (primitive-of name))
           (lower-call name '() (ast:node-type node)))
          ((lowered-global-p name)
           (make-ir-global :name name :type type :location *location*))
          ((typep node 'ast:node-local-variable)
           (make-ir-local :name (or (cdr (assoc name *renames*)) name) :type type :location *location*))
          (t (lowering-error "the reference to ~s, which is not specialized" name)))))

(defun literal-fit (name value)
  (handler-case (fit name value)
    (numeric-overflow ()
      (lowering-error "the literal ~d, which does not fit ~a" value (symbol-name name)))))

(defun error-panic (name rands type)
  "An `ir-panic` for a call of `error` NAME whose one argument is in RANDS, or NIL."
  (and (eq (or (specialized-origin *specialized* name) name) 'coalton/classes:error)
       (null (rest rands))
       (make-ir-panic :message (to-atom (first rands)) :type (lower-type type) :location *location*)))

(defun lower-call (name rands type)
  (or (error-panic name rands type)
      (lower-plain-call name rands type)))

(defun lower-plain-call (name rands type)
  (let ((args (mapcar #'to-atom rands))
        (type (lower-type type)))
    (multiple-value-bind (op arity) (primitive-of name)
      (cond ((constructor-p name)
             (make-ir-construct :constructor name :args args :type type :location *location*))
            (op
             (unless (= arity (length args))
               (lowering-error "a call of ~s with ~d arguments" name (length args)))
             (if (and (eq op :from-int) (typep (first args) 'ir-literal))
                 (make-ir-literal :value (literal-fit (ir-type-name type) (ir-literal-value (first args)))
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

(defun note-local-type (name type)
  (when *local-types*
    (setf (gethash name *local-types*) type)))

(defun lower-pattern (pattern)
  (let ((type (lower-type (pattern:pattern-type pattern))))
    (etypecase pattern
      (pattern:pattern-var
       (note-local-type (pattern:pattern-var-name pattern) type)
       (make-ir-pvar :name (pattern:pattern-var-name pattern) :type type))
      (pattern:pattern-wildcard (make-ir-pwild :type type))
      (pattern:pattern-literal (make-ir-plit :value (pattern:pattern-literal-value pattern) :type type))
      (pattern:pattern-binding
       (note-local-type (pattern:pattern-var-name (pattern:pattern-binding-var pattern)) type)
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

(defun note-exception-type (type)
  "TYPE, an exception type thrown or caught here, once it is known to be a data type."
  (unless (eq (ir-type-kind type) :data)
    (lowering-error "the exception type ~a, which has no constructors" (type-text type)))
  (push (list type (car *def*) *location*) *exception-sites*)
  type)

(defun lower-catch-branch (branch)
  "A catch branch: an exception type and a pattern for the exception, or neither
for a branch that catches every exception."
  (let* ((pattern (ast:catch-branch-pattern branch))
         (exception-type (ast:catch-branch-exception-type branch))
         (catch-all (and (null exception-type) (typep pattern 'pattern:pattern-wildcard))))
    (make-ir-catch-branch
     :type (and (not catch-all)
                (note-exception-type (lower-type (or exception-type (pattern:pattern-type pattern)))))
     :pattern (if catch-all (make-ir-pwild) (lower-pattern pattern))
     :body (lower-body (ast:catch-branch-body branch)))))

(defun lower-catch (node)
  (when (ast:node-catch-in-place-p node)
    (lowering-error "handle: https://github.com/takeiteasy/sheaf/issues/24"))
  (let ((*handler-depth* (1+ *handler-depth*)))
    (make-ir-catch :body (lower-body (ast:node-catch-expr node))
                   :branches (mapcar #'lower-catch-branch (ast:node-catch-branches node))
                   :type (lower-type (ast:node-type node)) :location *location*)))

(defun unit-node ()
  (make-ir-construct :constructor 'coalton:unit :args '() :type (lower-type tc:*unit-type*)
                     :location *location*))

(defun panic-message-form (form)
  "The message form of FORM, `(error 'panic :message MESSAGE)`, or NIL."
  (and (consp form) (eq (first form) 'error)
       (equal (second form) '(quote coalton/classes:panic))
       (eq (third form) :message)
       (null (nthcdr 4 form))
       (fourth form)))

(defun string-type ()
  (lower-type tc:*string-type*))

(defun string-node (text)
  (make-ir-literal :value text :type (string-type) :location *location*))

(defun bind-node (node)
  "A local holding NODE, which is bound first."
  (let ((name (make-symbol "T")))
    (emit name node)
    (make-ir-local :name name :type (ir-node-type node) :location *location*)))

(defun join-strings (atoms)
  "The concatenation of the `String` atoms ATOMS, as an atom."
  (if atoms
      (reduce (lambda (a b)
                (bind-node (make-ir-prim :op :string-concat :args (list a b) :type (string-type)
                                         :location *location*)))
              atoms)
      (string-node "")))

(defvar *printers* nil "The def that prints each data type, by type, for the program being lowered.")
(defvar *print-queue* '() "The printers asked for and not yet built, each its name and type.")

(defun printer-for (type)
  "The name of the def that prints the data TYPE."
  (or (gethash type *printers*)
      (let ((name (make-symbol (format nil "PRINT<~a>" (type-text type)))))
        (setf (gethash type *printers*) name)
        (push (cons name type) *print-queue*)
        name)))

(defun print-atom (atom directive)
  "A `String` atom for ATOM as the format directive DIRECTIVE, `#\\a` or `#\\s`, prints it."
  (let* ((type (ir-node-type atom))
         (name (ir-type-name type))
         (quoted (char= directive #\s)))
    (flet ((primitive (op)
             (bind-node (make-ir-prim :op op :args (list atom) :type (string-type) :location *location*))))
      (cond ((eq name 'coalton:string)
             (if quoted (join-strings (list (string-node "\"") atom (string-node "\""))) atom))
            ((eq name 'coalton:char)
             (let ((text (primitive :char-to-string)))
               (if quoted (join-strings (list (string-node "#\\") text)) text)))
            ((member (numeric-kind name) '(:wrap :trap)) (primitive :integer-to-string))
            ((eq (numeric-kind name) :float) (primitive :float-to-string))
            ((eq (ir-type-kind type) :data)
             (bind-node (make-ir-call :function (printer-for type) :args (list atom) :type (string-type)
                                      :location *location*)))
            (t (string-node (format nil "#<~a>" (symbol-name name))))))))

(defun format-parts (control)
  "The strings and directives of the format CONTROL as a list of strings and the
characters `a` and `s`, or `:unsupported` for a directive other than `~a`, `~d`,
`~s`, `~%` and `~~`."
  (let ((parts '())
        (text (make-string-output-stream))
        (i 0))
    (flet ((flush ()
             (let ((string (get-output-stream-string text)))
               (when (plusp (length string)) (push string parts)))))
      (loop :while (< i (length control))
            :do (let ((char (char control i)))
                  (incf i)
                  (if (char/= char #\~)
                      (write-char char text)
                      (let ((directive (and (< i (length control)) (char-downcase (char control i)))))
                        (incf i)
                        (case directive
                          (#\% (write-char #\Newline text))
                          (#\~ (write-char #\~ text))
                          ((#\a #\d #\s) (flush) (push (if (char= directive #\s) #\s #\a) parts))
                          (t (return-from format-parts :unsupported)))))))
      (flush))
    (nreverse parts)))

(defun lower-format (control args)
  "A `String` atom for the format CONTROL given the atoms ARGS, or NIL when
CONTROL has a directive that is not supported or does not match ARGS."
  (let ((parts (format-parts control)))
    (when (and (listp parts) (= (count-if #'characterp parts) (length args)))
      (join-strings (loop :for part :in parts
                          :collect (if (stringp part)
                                       (string-node part)
                                       (print-atom (pop args) part)))))))

(defun var-local (vars symbol)
  "The local the raw Lisp variable SYMBOL of VARS names, or NIL if its type is not known."
  (let* ((name (cdr (assoc symbol vars)))
         (type (and name (gethash name *local-types*))))
    (and type (make-ir-local :name name :type type :location *location*))))

(defun message-atom (vars form)
  "A `String` atom for FORM, a string or a `format` to nil of a control string
and variables of VARS, or NIL."
  (typecase form
    (string (string-node form))
    (cons (and (eq (first form) 'format) (null (second form)) (stringp (third form))
               (every #'symbolp (cdddr form))
               (let ((args (mapcar (lambda (arg) (var-local vars arg)) (cdddr form))))
                 (and (every #'identity args)
                      (lower-format (third form) args)))))))

(defun escape-directives (text)
  "TEXT as format control that prints it."
  (with-output-to-string (out)
    (loop :for char :across text
          :do (when (char= char #\~) (write-char #\~ out))
              (write-char char out))))

(defun lower-assert (vars form type)
  "The `ir-match` of FORM, the raw Lisp of an `assert`, or NIL for other raw Lisp."
  (let* ((statement (first form))
         (datum-var (and (consp statement) (second statement)))
         (message (and (null (rest form)) (consp statement) (eq (first statement) 'unless)
                       (symbolp datum-var) (assoc datum-var vars) (null (nthcdr 3 statement))
                       (panic-message-form (third statement)))))
    (when (and (consp message) (eq (first message) 'format) (= (length message) 6)
               (equal (third message) "Assertion ~A failed: ~?")
               (stringp (fifth message))
               (consp (sixth message)) (eq (first (sixth message)) 'list)
               (every #'symbolp (rest (sixth message))))
      (let ((args (mapcar (lambda (arg) (var-local vars arg)) (rest (sixth message))))
            (control (concatenate 'string
                                  (escape-directives (with-standard-io-syntax
                                                       (format nil "Assertion ~A failed: " (second (fourth message)))))
                                  (fifth message)))
            (boolean (lower-type tc:*boolean-type*)))
        (when (every #'identity args)
          (let* ((*bindings* '())
                 (text (lower-format control args)))
            (when text
              (flet ((branch (constructor body)
                       (make-ir-branch :pattern (make-ir-pcon :constructor constructor :patterns '() :type boolean)
                                       :body body)))
                (make-ir-match
                 :scrutinee (make-ir-local :name (cdr (assoc datum-var vars)) :type boolean :location *location*)
                 :type type :location *location*
                 :branches (list (branch 'coalton:true (unit-node))
                                 (branch 'coalton:false
                                         (wrap-bindings (make-ir-panic :message text :type type
                                                                       :location *location*)))))))))))))

(defun lower-lisp (node)
  "NODE, raw Lisp, as an `ir-panic`, the `ir-match` of an `assert` or the text of
a `format` when it is one of those, otherwise as `ir-foreign`."
  (let* ((vars (ast:node-lisp-vars node))
         (form (ast:node-lisp-form node))
         (type (lower-type (ast:node-type node)))
         (single (and (null (rest form)) (first form)))
         (panic (and single (panic-message-form single))))
    (or (and panic
             (let ((message (message-atom vars panic)))
               (and message (make-ir-panic :message message :type type :location *location*))))
        (lower-assert vars form type)
        (and single (not panic) (eq (ir-type-name type) 'coalton:string) (message-atom vars single))
        (make-ir-foreign :vars vars :form form :type type :location *location*))))

(defun lower-discarded (node)
  "The body that computes NODE, binds its value and gives Unit, for code run for its effects."
  (let ((*bindings* '()))
    (emit (make-symbol "_") (lower-simple node))
    (wrap-bindings (unit-node))))

(defun lower-for (node)
  (let* ((bindings (ast:node-for-bindings node))
         (names (mapcar #'ast:node-for-binding-name bindings))
         (kind (ast:node-for-termination-kind node))
         (termination (ast:node-for-termination-expr node))
         (label (make-symbol (symbol-name (ast:node-for-label node))))
         (counter (and (eq kind :repeat) (make-symbol "REPEAT")))
         (renames '())
         (inits '())
         (vars (loop :for binding :in bindings
                     :collect (cons (ast:node-for-binding-name binding)
                                    (lower-type (ast:node-for-binding-type binding))))))
    (flet ((bound-init (name node)
             (let* ((simple (let ((*renames* renames)) (lower-simple node)))
                    (local (make-symbol (symbol-name name))))
               (emit local simple)
               (make-ir-local :name local :type (ir-node-type simple) :location (ir-node-location simple)))))
      (dolist (binding bindings)
        (unless (ast:node-for-sequential-p node)
          (when (intersection names (referenced-names (ast:node-for-binding-init binding)))
            (lowering-error "a loop variable initialized from another one")))
        (let ((init (bound-init (ast:node-for-binding-name binding) (ast:node-for-binding-init binding))))
          (push (cons (ast:node-for-binding-name binding) (ir-local-name init)) renames)
          (push init inits)))
      (when counter
        (let ((init (bound-init counter termination)))
          (push init inits)
          (setf vars (append vars (list (cons counter (ir-node-type init)))))))
      (setf inits (nreverse inits))
      (let* ((count-type (and counter (cdr (car (last vars)))))
             (count (and counter (make-ir-local :name counter :type count-type :location *location*)))
             (steps (append (loop :for binding :in bindings
                                  :collect (and (ast:node-for-binding-step binding)
                                                (lower-body (ast:node-for-binding-step binding))))
                            (and counter
                                 (list (make-ir-prim :op :sub :type count-type :location *location*
                                                     :args (list count (make-ir-literal :value 1 :type count-type
                                                                                        :location *location*)))))))
             (test (cond (counter
                          (make-ir-prim :op :eq :type (lower-type tc:*boolean-type*) :location *location*
                                        :args (list count (make-ir-literal :value 0 :type count-type
                                                                           :location *location*))))
                         (termination (lower-body termination))))
             (body (let ((*loop-labels* (acons (ast:node-for-label node) (cons label *handler-depth*) *loop-labels*)))
                     (lower-discarded (ast:node-for-body node))))
             (returns (if (ast:node-for-returns node)
                          (lower-body (ast:node-for-returns node))
                          (unit-node))))
        (make-ir-loop :label label :vars vars :inits inits :sequential (ast:node-for-sequential-p node)
                      :test test :exit-on (if (eq kind :while) :false :true)
                      :steps steps :body body :returns returns
                      :type (lower-type (ast:node-type node)) :location *location*)))))

(defun lower-exit (node label maker)
  "The break or continue NODE of the loop named LABEL, made by MAKER."
  (let ((target (cdr (assoc label *loop-labels*))))
    (unless target
      (lowering-error "a break or continue outside its loop"))
    (unless (= (cdr target) *handler-depth*)
      (lowering-error "a break or continue out of a catch or protect: https://github.com/takeiteasy/sheaf/issues/40"))
    (funcall maker :label (car target) :type (lower-type (ast:node-type node)) :location *location*)))

(defun lower-block (node)
  (let* ((name (ast:node-block-name node))
         (label (make-symbol (symbol-name name)))
         (*return-targets* (acons name (cons label *handler-depth*) *return-targets*)))
    (make-ir-block :label label :body (lower-body (ast:node-block-body node))
                   :type (lower-type (ast:node-type node)) :location *location*)))

(defun lower-return (node)
  (destructuring-bind (label . depth) (cdr (assoc (ast:node-return-from-name node) *return-targets*))
    (unless (= depth *handler-depth*)
      (lowering-error "a return out of a catch or protect: https://github.com/takeiteasy/sheaf/issues/40"))
    (make-ir-return :label label :value (to-atom (ast:node-return-from-expr node))
                    :type (lower-type (ast:node-type node)) :location *location*)))

(defun lower-simple (node)
  "The IR of NODE as an atom or one simple expression, after emitting a binding
for everything it evaluates first."
  (let ((*location* (or (ast:node-location node) *location*)))
    (etypecase node
      (ast:node-literal
       (let ((value (ast:node-literal-value node)))
         (make-ir-literal :value (if (characterp value) (char-code value) value)
                          :type (lower-type (ast:node-type node)) :location *location*)))
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
      (ast:node-throw
       (let ((value (to-atom (ast:node-throw-expr node))))
         (note-exception-type (ir-node-type value))
         (make-ir-throw :value value :type (lower-type (ast:node-type node)) :location *location*)))
      (ast:node-protect
       (let ((*handler-depth* (1+ *handler-depth*)))
         (make-ir-protect :body (lower-body (ast:node-protect-expr node))
                          :cleanup (lower-body (ast:node-protect-cleanup node))
                          :type (lower-type (ast:node-type node)) :location *location*)))
      (ast:node-for (lower-for node))
      (ast:node-break (lower-exit node (ast:node-break-label node) #'make-ir-break))
      (ast:node-continue (lower-exit node (ast:node-continue-label node) #'make-ir-continue))
      (ast:node-block (lower-block node))
      (ast:node-return-from (lower-return node))
      (ast:node-catch (lower-catch node))
      (ast:node-values
       (let ((nodes (ast:node-values-nodes node)))
         (cond ((null nodes)
                (unit-node))
               ((null (rest nodes)) (lower-simple (first nodes)))
               (t (lowering-error "multiple values: https://github.com/takeiteasy/sheaf/issues/39")))))
      (ast:node-lisp (lower-lisp node))
      (ast:node-abstraction (lowering-error "a lambda below the top level")))))

(defun lower-def (name node)
  (let* ((*local-types* (make-hash-table :test 'eq))
         (*def* (cons name (ast:node-type node)))
         (*location* (ast:node-location node))
         (origin (specialized-origin *specialized* name)))
    (if (typep node 'ast:node-abstraction)
        (let ((type (ast:node-type node)))
          (when (ast:node-abstraction-keyword-params node)
            (lowering-error "keyword arguments"))
          (make-ir-def name origin :function
                       (loop :for var :in (ast:node-abstraction-vars node)
                             :for param :in (tc:function-type-arguments type)
                             :collect (let ((param-type (lower-type param)))
                                        (note-local-type var param-type)
                                        (cons var param-type)))
                       (lower-type (tc:function-return-type type))
                       (lower-body (ast:node-abstraction-subexpr node))
                       *location*))
        (make-ir-def name origin :value '()
                     (lower-type (ast:node-type node))
                     (lower-body node)
                     *location*))))

(defun check-exception-payloads (lowered)
  "Signal an error for an exception type that holds storage or a reference, or
that contains itself: an exception is copied by value when thrown, and what a
thrown value points to is freed while it unwinds."
  (loop :for (exception name location) :in (reverse *exception-sites*)
        :do (labels ((fail (what)
                       (error 'specialization-error
                              :message (format nil "Lowering does not support the exception type ~a, which holds ~a"
                                               (type-text exception) what)
                              :chain (list (list name (type-text exception) location))))
                     (check (type path)
                       (ecase (ir-type-kind type)
                         (:scalar)
                         ((:storage :view) (fail (type-text type)))
                         (:data
                          (when (member type path)
                            (fail (format nil "~a, which contains itself" (type-text type))))
                          (dolist (constructor (lowered-constructors lowered type))
                            (dolist (field (ir-constructor-fields constructor))
                              (check field (cons type path))))))))
              (check exception '()))))

(defun drop-unused-errors (lowered)
  "Remove the defs of `error` that no call refers to, since every call of one is an `ir-panic`."
  (let ((used (make-hash-table :test 'eq))
        (defs (lowered-defs lowered)))
    (maphash (lambda (name def)
               (declare (ignore name))
               (walk-ir (lambda (node)
                          (typecase node
                            (ir-call (setf (gethash (ir-call-function node) used) t))
                            (ir-global (setf (gethash (ir-global-name node) used) t))))
                        (ir-def-body def)))
             defs)
    (loop :for name :being :the :hash-keys :of defs :using (:hash-value def)
          :when (and (eq (ir-def-origin def) 'coalton/classes:error) (not (gethash name used)))
            :do (remhash name defs))))

(defun printer-branch (type constructor)
  "The branch of the printer of TYPE that prints CONSTRUCTOR as SBCL prints it, `#.(SOME 1)` or `#.NONE`."
  (let* ((*bindings* '())
         (fields (loop :for field :in (ir-constructor-fields constructor)
                       :collect (make-ir-local :name (make-symbol "F") :type field)))
         (name (symbol-name (ir-constructor-name constructor)))
         (text (cond ((eq (ir-type-name type) 'coalton:boolean)
                      (string-node (if (string= name "TRUE") "T" "NIL")))
                     ((null fields) (string-node (format nil "#.~a" name)))
                     (t (join-strings (append (list (string-node (format nil "#.(~a" name)))
                                              (loop :for field :in fields
                                                    :append (list (string-node " ") (print-atom field #\a)))
                                              (list (string-node ")"))))))))
    (make-ir-branch :pattern (make-ir-pcon :constructor (ir-constructor-name constructor)
                                           :patterns (loop :for field :in fields
                                                           :collect (make-ir-pvar :name (ir-local-name field)
                                                                                  :type (ir-node-type field)))
                                           :type type)
                    :body (wrap-bindings text))))

(defun printer-def (lowered name type)
  "The def NAME that prints a value of the data TYPE."
  (let ((value (make-symbol "VALUE")))
    (make-ir-def name nil :function (list (cons value type)) (string-type)
                 (make-ir-match :scrutinee (make-ir-local :name value :type type)
                                :type (string-type)
                                :branches (loop :for constructor :in (lowered-constructors lowered type)
                                                :collect (printer-branch type constructor)))
                 nil)))

(defun build-printers (lowered)
  "Build the type table of LOWERED and the printers lowering asked for, which
may need more types and more printers."
  (loop
    (build-type-table lowered)
    (when (null *print-queue*)
      (return))
    (dolist (request (shiftf *print-queue* '()))
      (destructuring-bind (name . type) request
        (setf (gethash name (lowered-defs lowered)) (printer-def lowered name type))))))

(defun lower (specialized)
  "SPECIALIZED, a defunctionalized program, in Sheaf's IR."
  (check-defunctionalized specialized)
  (let* ((entry:*global-environment* (specialized-environment specialized))
         (*specialized* specialized)
         (*ir-types* (make-hash-table :test 'equal))
         (*pending-types* '())
         (*printers* (make-hash-table :test 'eq))
         (*print-queue* '())
         (*exception-sites* '())
         (*prelude-primitives* (prelude-primitives))
         (*numeric-primitives* (numeric-primitives))
         (lowered (%make-lowered :root (specialized-root specialized))))
    (maphash (lambda (name node)
               (unless (or (primitive-of name) (gethash name (specialized-passthrough specialized)))
                 (setf (gethash name (lowered-defs lowered)) (lower-def name node))))
             (specialized-bindings specialized))
    (drop-unused-errors lowered)
    (build-printers lowered)
    (check-exception-payloads lowered)
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
                            (append (check-pattern (ir-branch-pattern branch)) scope))))
                  (ir-panic (check-atom (ir-panic-message node) scope))
                  (ir-throw (check-atom (ir-throw-value node) scope))
                  (ir-catch
                   (check (ir-catch-body node) scope)
                   (dolist (branch (ir-catch-branches node))
                     (check (ir-catch-branch-body branch)
                            (append (check-pattern (ir-catch-branch-pattern branch)) scope))))
                  (ir-protect
                   (check (ir-protect-body node) scope)
                   (check (ir-protect-cleanup node) scope))
                  (ir-block
                   (let ((*exit-labels* (cons (ir-block-label node) *exit-labels*)))
                     (check (ir-block-body node) scope)))
                  ((or ir-break ir-continue)
                   (unless (member (if (ir-break-p node) (ir-break-label node) (ir-continue-label node)) *exit-labels*)
                     (fail "a break or continue leaves no loop")))
                  (ir-loop
                   (dolist (init (ir-loop-inits node)) (check-atom init scope))
                   (let ((scope (append (mapcar #'car (ir-loop-vars node)) scope)))
                     (when (ir-loop-test node) (check (ir-loop-test node) scope))
                     (dolist (step (ir-loop-steps node))
                       (when step (check step scope)))
                     (let ((*exit-labels* (cons (ir-loop-label node) *exit-labels*)))
                       (check (ir-loop-body node) scope))
                     (check (ir-loop-returns node) scope)))
                  (ir-return
                   (unless (member (ir-return-label node) *exit-labels*)
                     (fail "a return leaves no block"))
                   (check-atom (ir-return-value node) scope)))))
       (check (ir-def-body def) (mapcar #'car (ir-def-params def)))))
   (lowered-defs lowered))
  t)
