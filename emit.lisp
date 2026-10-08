;;;; emit.lisp
;;;; Emitting an assigned program as one C file that includes runtime/sheaf.h.

(in-package #:sheaf)

(defvar *out*)
(defvar *indent* 0)
(defvar *c-types*)
(defvar *c-names*)
(defvar *c-counter*)
(defvar *emit-def* nil "The def being emitted, for errors.")
(defvar *self*)
(defvar *params*)
(defvar *pool*)
(defvar *result-c-type*)
(defvar *tail-used*)

(defun emit-error (control &rest args)
  (let ((location (and *emit-def* (ir-def-location *emit-def*))))
    (error "The C emitter does not support ~?~@[ in ~a~]~@[, at offset ~d~]"
           control args
           (and *emit-def* (or (ir-def-origin *emit-def*) (ir-def-name *emit-def*)))
           (and location (source:span-start (source:location-span location))))))

;;; Output

(defun line (control &rest args)
  (dotimes (i *indent*) (write-string "  " *out*))
  (apply #'format *out* control args)
  (terpri *out*))

(defmacro indented (&body body)
  `(let ((*indent* (1+ *indent*))) ,@body))

(defun comment (text)
  (line "// ~a" (substitute #\Space #\Newline text)))

;;; Names

(defun fresh-c-name (symbol)
  "A C identifier for SYMBOL, different from every other one."
  (let* ((text (substitute-if #\_ (lambda (char) (not (and (< (char-code char) 128) (alphanumericp char))))
                              (symbol-name symbol)))
         (text (subseq text 0 (min 20 (length text)))))
    (format nil "~:[~;v~]~(~a~)_~d"
            (or (zerop (length text)) (digit-char-p (char text 0)))
            text (incf *c-counter*))))

(defun c-name (name)
  (or (gethash name *c-names*) (error "~s is not a def" name)))

;;; Types

(defun ufix-type-p (type)
  (eq (ir-type-name type) (find-symbol "UFIX" "COALTON")))

(defun struct-name (type)
  (format nil "T~d" (or (gethash type *c-types*)
                        (setf (gethash type *c-types*) (hash-table-count *c-types*)))))

(defun c-type (type)
  (ecase (ir-type-kind type)
    (:scalar (if (ufix-type-p type)
                 "uint64_t"
                 (emit-error "the type ~a: https://todo.sr.ht/~~takeiteasy/sheaf/23" (type-text type))))
    (:reference (emit-error "the reference type ~a: https://todo.sr.ht/~~takeiteasy/sheaf/8"
                            (type-text type)))
    ((:storage :view) (emit-error "the array or reference type ~a: https://todo.sr.ht/~~takeiteasy/sheaf/27"
                                  (type-text type)))
    (:data (format nil "struct ~a~:[~; *~]" (struct-name type) (pooled-p type)))))

(defun emit-type-definitions (lowered)
  (let ((done (make-hash-table :test 'eq))
        (types (loop :for type :being :the :hash-keys :of (lowered-types lowered) :collect type)))
    (dolist (type types)
      (line "struct ~a;" (struct-name type)))
    (labels ((define (type)
               (unless (gethash type done)
                 (setf (gethash type done) t)
                 (dolist (field (field-types type))
                   (when (and (eq (ir-type-kind field) :data) (not (pooled-p field)))
                     (define field)))
                 (let ((constructors (lowered-constructors lowered type)))
                   (comment (type-text type))
                   (line "struct ~a {" (struct-name type))
                   (indented
                     (line "uint32_t tag;")
                     (when (some #'ir-constructor-fields constructors)
                       (line "union {")
                       (indented
                         (dolist (constructor constructors)
                           (when (ir-constructor-fields constructor)
                             (line "struct { ~{~a~^ ~} } c~d;"
                                   (loop :for field :in (ir-constructor-fields constructor)
                                         :for i :from 0
                                         :collect (format nil "~a f~d;" (c-type field) i))
                                   (ir-constructor-tag constructor)))))
                       (line "} u;")))
                   (line "};")))))
      (mapc #'define types))))

;;; Expressions

(defun c-literal (value type)
  (unless (and (integerp value) (ufix-type-p type) (< -1 value (expt 2 64)))
    (emit-error "the literal ~s of type ~a" value (type-text type)))
  (format nil "UINT64_C(~d)" value))

(defun atom-expr (node env)
  (etypecase node
    (ir-literal (c-literal (ir-literal-value node) (ir-node-type node)))
    (ir-local (or (cdr (assoc (ir-local-name node) env))
                  (error "~s is not bound" (ir-local-name node))))
    (ir-global (format nil "~a()" (c-name (ir-global-name node))))))

(defun constructor-of (name type)
  (find name (lowered-constructors *lowered* type) :key #'ir-constructor-name))

;; TODO: the cells of a recursive type are linked by C pointers, where
;; docs/MEMORY.md describes relative references.
;; https://todo.sr.ht/~takeiteasy/sheaf/26
(defun member-access (access type member)
  (format nil (if (pooled-p type) "(~a)->~a" "(~a).~a") access member))

(defun prim-expr (node env)
  (when (prelude-op-p (ir-prim-op node))
    (emit-error "the array operation ~(~a~): https://todo.sr.ht/~~takeiteasy/sheaf/27" (ir-prim-op node)))
  (destructuring-bind (&optional a b) (mapcar (lambda (arg) (atom-expr arg env)) (ir-prim-args node))
    (let ((type (ir-node-type node)))
      (flet ((tag (name) (ir-constructor-tag (named-constructor *lowered* type name)))
             (boolean (test)
               (format nil "(~a){ .tag = ~a ? ~d : ~d }" (c-type type) test
                       (ir-constructor-tag (named-constructor *lowered* type "TRUE"))
                       (ir-constructor-tag (named-constructor *lowered* type "FALSE")))))
        (ecase (ir-prim-op node)
          (:ufix-add (format nil "(~a + ~a)" a b))
          (:ufix-sub (format nil "(~a - ~a)" a b))
          (:ufix-mul (format nil "(~a * ~a)" a b))
          (:ufix-eq (boolean (format nil "~a == ~a" a b)))
          (:ufix-compare (format nil "(~a){ .tag = ~a < ~a ? ~d : ~a > ~a ? ~d : ~d }"
                                 (c-type type) a b (tag "LT") a b (tag "GT") (tag "EQ")))
          (:ordering-eq (boolean (format nil "~a.tag == ~a.tag" a b)))
          (:ufix-from-int (emit-error "a conversion of a value that is not a literal: https://todo.sr.ht/~~takeiteasy/sheaf/9")))))))

(defun construct-expr (node env)
  (let* ((type (ir-node-type node))
         (constructor (constructor-of (ir-construct-constructor node) type))
         (tag (ir-constructor-tag constructor))
         (args (mapcar (lambda (arg) (atom-expr arg env)) (ir-construct-args node))))
    (if (pooled-p type)
        (let ((cell (fresh-c-name (make-symbol "CELL")))
              (struct (format nil "struct ~a" (struct-name type))))
          (line "~a *~a = sheaf_push(&sheaf_stacks[~d], sizeof(~a), _Alignof(~a));"
                struct cell (ir-construct-stack node) struct struct)
          (line "~a->tag = ~d;" cell tag)
          (loop :for arg :in args
                :for i :from 0
                :do (line "~a->u.c~d.f~d = ~a;" cell tag i arg))
          cell)
        (format nil "(~a){ .tag = ~d~@[, .u.c~d = { ~{~a~^, ~} }~] }"
                (c-type type) tag (and args tag) args))))

(defun call-expr (node env)
  (let ((call (format nil "~a(~{~a~^, ~})" (c-name (ir-call-function node))
                      (mapcar (lambda (arg) (atom-expr arg env)) (ir-call-args node))))
        (region (ir-call-region node)))
    (if region
        (let ((mark (fresh-c-name (make-symbol "MARK")))
              (result (fresh-c-name (make-symbol "RESULT"))))
          (line "char *~a = sheaf_stacks[~d].top;" mark region)
          (line "~a ~a = ~a;" (c-type (ir-node-type node)) result call)
          (line "sheaf_stacks[~d].top = ~a;" region mark)
          result)
        call)))

(defun simple-expr (node env)
  "A C expression for the simple NODE, after writing any statements it needs."
  (etypecase node
    (ir-atom (atom-expr node env))
    (ir-prim (prim-expr node env))
    (ir-construct (construct-expr node env))
    (ir-call (call-expr node env))
    (ir-foreign (emit-error "raw Lisp: https://todo.sr.ht/~~takeiteasy/sheaf/16"))))

;;; Statements

(defun emit-return (expression)
  (if *pool*
      (let ((result (fresh-c-name (make-symbol "RESULT"))))
        (line "~a ~a = ~a;" *result-c-type* result expression)
        (line "sheaf_stacks[~d].top = pool_mark;" *pool*)
        (line "return ~a;" result))
      (line "return ~a;" expression)))

(defun self-tail-call-p (node)
  (and (typep node 'ir-call)
       (eq (ir-call-function node) *self*)
       (null (ir-call-region node))
       (null *pool*)))

(defun emit-tail-call (node env)
  (setf *tail-used* t)
  (line "{")
  (indented
    (let ((temps (loop :for arg :in (ir-call-args node)
                       :for (nil . type) :in *params*
                       :collect (let ((temp (fresh-c-name (make-symbol "ARG"))))
                                  (line "~a ~a = ~a;" type temp (atom-expr arg env))
                                  temp))))
      (loop :for (name) :in *params*
            :for temp :in temps
            :do (line "~a = ~a;" name temp)))
    (line "goto top;"))
  (line "}"))

(defun pattern-code (pattern access)
  "The C tests that ACCESS matches PATTERN and the variables it binds, each a
symbol, a type and the expression for its value."
  (etypecase pattern
    (ir-pwild (values '() '()))
    (ir-pvar (values '() (list (list (ir-pvar-name pattern) (ir-pattern-type pattern) access))))
    (ir-plit (values (list (format nil "~a == ~a" access
                                   (c-literal (ir-plit-value pattern) (ir-pattern-type pattern))))
                     '()))
    (ir-pas (multiple-value-bind (tests bindings) (pattern-code (ir-pas-pattern pattern) access)
              (values tests (cons (list (ir-pas-name pattern) (ir-pattern-type pattern) access)
                                  bindings))))
    (ir-pcon
     (let* ((type (ir-pattern-type pattern))
            (tag (ir-constructor-tag (constructor-of (ir-pcon-constructor pattern) type)))
            (tests (list (format nil "~a == ~d" (member-access access type "tag") tag)))
            (bindings '()))
       (loop :for field :in (ir-pcon-patterns pattern)
             :for i :from 0
             :do (multiple-value-bind (field-tests field-bindings)
                     (pattern-code field (member-access access type (format nil "u.c~d.f~d" tag i)))
                   (setf tests (append tests field-tests)
                         bindings (append bindings field-bindings))))
       (values tests bindings)))))

(defun emit-match (node target env)
  (let* ((scrutinee (ir-match-scrutinee node))
         (access (if (typep scrutinee 'ir-global)
                     (let ((copy (fresh-c-name (make-symbol "SCRUTINEE"))))
                       (line "~a ~a = ~a;" (c-type (ir-node-type scrutinee)) copy (atom-expr scrutinee env))
                       copy)
                     (atom-expr scrutinee env))))
    (loop :for branch :in (ir-match-branches node)
          :for first := t :then nil
          :do (multiple-value-bind (tests bindings) (pattern-code (ir-branch-pattern branch) access)
                (line "~:[} else ~;~]if (~{~a~^ && ~}) {" first (or tests '("1")))
                (indented
                  (let ((branch-env env))
                    (loop :for (name type expression) :in bindings
                          :do (let ((c-name (fresh-c-name name)))
                                (line "~a ~a SHEAF_UNUSED = ~a;" (c-type type) c-name expression)
                                (push (cons name c-name) branch-env)))
                    (emit-node (ir-branch-body branch) target branch-env)))))
    (cond ((ir-match-branches node)
           (line "} else {")
           (indented (line "sheaf_no_match();"))
           (line "}"))
          (t (line "sheaf_no_match();")))))

(defun emit-node (node target env)
  "Write the statements that compute NODE into TARGET, a C variable or `:return`."
  (etypecase node
    (ir-let
     (let ((name (fresh-c-name (ir-let-name node))))
       (line "~a ~a SHEAF_UNUSED;" (c-type (ir-node-type (ir-let-value node))) name)
       (emit-node (ir-let-value node) name env)
       (emit-node (ir-let-body node) target (acons (ir-let-name node) name env))))
    (ir-match (emit-match node target env))
    ((or ir-atom ir-prim ir-construct ir-call ir-foreign)
     (cond ((and (eq target :return) (self-tail-call-p node))
            (emit-tail-call node env))
           (t (let ((expression (simple-expr node env)))
                (if (eq target :return)
                    (emit-return expression)
                    (line "~a = ~a;" target expression))))))))

;;; Definitions

(defun signature (def name params)
  (format nil "static SHEAF_UNUSED ~a ~a(~:[void~;~:*~{~a~^, ~}~])"
          (c-type (ir-def-result def)) name params))

(defun emit-def (def)
  (let* ((*emit-def* def)
         (*self* (ir-def-name def))
         (*tail-used* nil)
         (*result-c-type* (c-type (ir-def-result def)))
         (*pool* (ir-def-pool def))
         (name (c-name (ir-def-name def)))
         (params (loop :for (symbol . type) :in (ir-def-params def)
                       :collect (cons (fresh-c-name symbol) (c-type type))))
         (*params* params)
         (env (loop :for (symbol) :in (ir-def-params def)
                    :for (c-name) :in params
                    :collect (cons symbol c-name)))
         (value-p (eq (ir-def-kind def) :value))
         (body (with-output-to-string (*out*)
                 (let ((*indent* 1))
                   (when *pool*
                     (line "char *pool_mark = sheaf_stacks[~d].top;" *pool*))
                   (cond (value-p
                          (emit-node (ir-def-body def) "value" env)
                          (when *pool*
                            (line "sheaf_stacks[~d].top = pool_mark;" *pool*))
                          (line "ready = 1;")
                          (line "return value;"))
                         (t (emit-node (ir-def-body def) :return env)))))))
    (comment (symbol-name (ir-def-name def)))
    (line "~a {" (signature def name (loop :for (c-name . type) :in params
                                           :collect (format nil "~a ~a SHEAF_UNUSED" type c-name))))
    (indented
      (when value-p
        (line "static ~a value;" *result-c-type*)
        (line "static int ready;")
        (line "if (ready) return value;")))
    (when *tail-used*
      (indented (line "top: ;")))
    (write-string body *out*)
    (line "}")
    (terpri *out*)))

(defun emit-main (lowered)
  (let* ((root (gethash (lowered-root lowered) (lowered-defs lowered)))
         (*emit-def* root)
         (call (format nil "~a()" (c-name (lowered-root lowered)))))
    (when (ir-def-params root)
      (emit-error "a root that takes arguments"))
    (line "int main(void) {")
    (indented
      (line "sheaf_reserve(sheaf_stacks, ~d);" (lowered-stack-count lowered))
      (cond ((ufix-type-p (ir-def-result root))
             (line "printf(\"%llu\\n\", (unsigned long long)~a);" call))
            (t (line "(void)~a;" call)))
      (line "return 0;"))
    (line "}")))

(defun emit-c (assigned &optional stream)
  "ASSIGNED, a program with its data stacks assigned, as a C file that includes
runtime/sheaf.h. Writes it to STREAM, or returns it as a string without one."
  (check-assigned assigned)
  (if stream
      (let ((*out* stream)
            (*indent* 0)
            (*lowered* assigned)
            (*c-types* (make-hash-table :test 'eq))
            (*c-names* (make-hash-table :test 'eq))
            (*c-counter* 0)
            (*emit-def* nil))
        (maphash (lambda (name def)
                   (declare (ignore def))
                   (setf (gethash name *c-names*) (fresh-c-name name)))
                 (lowered-defs assigned))
        (line "// Generated by Sheaf.")
        (line "#include \"sheaf.h\"")
        (terpri *out*)
        (emit-type-definitions assigned)
        (terpri *out*)
        (line "static sheaf_stack sheaf_stacks[~d];" (max 1 (lowered-stack-count assigned)))
        (terpri *out*)
        (maphash (lambda (name def)
                   (let ((*emit-def* def))
                     (line "~a;" (signature def (c-name name)
                                            (loop :for (nil . type) :in (ir-def-params def)
                                                  :collect (c-type type))))))
                 (lowered-defs assigned))
        (terpri *out*)
        (maphash (lambda (name def)
                   (declare (ignore name))
                   (emit-def def))
                 (lowered-defs assigned))
        (emit-main assigned))
      (with-output-to-string (stream)
        (emit-c assigned stream))))

(defun write-c (assigned pathname)
  "Write ASSIGNED to the C file PATHNAME. Returns PATHNAME."
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (emit-c assigned stream))
  pathname)
