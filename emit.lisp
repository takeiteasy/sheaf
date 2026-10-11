;;;; emit.lisp
;;;; Emitting an assigned program as one C file that includes runtime/sheaf.h.

(in-package #:sheaf)

(defvar *out*)
(defvar *indent* 0)
(defvar *c-types*)
(defvar *c-arrays*)
(defvar *c-names*)
(defvar *c-counter*)
(defvar *emit-def* nil "The def being emitted, for errors.")
(defvar *self*)
(defvar *params*)
(defvar *releases* '() "What to give back on return, each `:stack` or `:bank`, its index and the C variable holding its mark.")
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

(defun struct-name (type)
  (format nil "T~d" (or (gethash type *c-types*)
                        (setf (gethash type *c-types*) (hash-table-count *c-types*)))))

(defun storage-layout (type)
  "How TYPE is laid out in C: `:array`, `:grow`, `:table`, `:slice`, `:pointer`, or nil."
  (let ((name (ir-type-name type)))
    (cond ((member name '(sheaf/prelude:fixedarray sheaf/prelude:array)) :array)
          ((member name '(sheaf/prelude:growarray sheaf/prelude:stack coalton/vector:vector)) :grow)
          ((eq name 'coalton/hashtable:hashtable) :table)
          ((eq name 'sheaf/prelude:slice) :slice)
          ((member name '(sheaf/prelude:ref coalton/cell:cell)) :pointer))))

(defun element-type (type)
  (first (last (ir-type-args type))))

(defun array-struct (layout type)
  "The name of the C struct for LAYOUT, `:array`, `:grow`, `:table` or `:slice`, of elements like TYPE's."
  (let* ((key (list layout (c-type (element-type type))))
         (id (or (gethash key *c-arrays*)
                 (setf (gethash key *c-arrays*) (hash-table-count *c-arrays*)))))
    (format nil "~a~d" (ecase layout (:array "A") (:grow "G") (:table "H") (:slice "S")) id)))

(defun c-type (type)
  (ecase (ir-type-kind type)
    (:scalar (or (numeric-c-type (ir-type-name type))
                 (emit-error "the type ~a: https://github.com/takeiteasy/sheaf/issues/22" (type-text type))))
    ((:storage :view)
     (let ((layout (storage-layout type)))
       (case layout
         ((:array :grow :table) (format nil "struct ~a *" (array-struct layout type)))
         (:slice (format nil "struct ~a" (array-struct layout type)))
         (:pointer (format nil "~a *" (c-type (element-type type)))))))
    (:data (format nil "struct ~a~:[~; *~]" (struct-name type) (pooled-p type)))))

(defun emit-forward-declarations ()
  (maphash (lambda (type id) (declare (ignore type)) (line "struct T~d;" id)) *c-types*)
  (maphash (lambda (key id)
             (line "struct ~a~d;" (ecase (first key) (:array "A") (:grow "G") (:table "H") (:slice "S")) id))
           *c-arrays*))

(defun emit-array-definitions (layouts)
  "The C structs of the array layouts LAYOUTS, in the order they were first used."
  (let ((entries '()))
    (maphash (lambda (key id) (when (member (first key) layouts) (push (cons id key) entries))) *c-arrays*)
    (loop :for (id layout element) :in (sort entries #'< :key #'car) :do
      (ecase layout
        (:slice (line "struct S~d { ~a *e; uint64_t length; };" id element))
        (:array (line "struct A~d { uint64_t length; ~a e[]; };" id element))
        (:grow (line "struct G~d { sheaf_stack *stack; uint64_t length; ~a e[]; };" id element))
        (:table (line "struct H~d { sheaf_table table; struct { uint64_t key, used; ~a value; } e[]; };" id element))))))

(defun emit-type-definitions (lowered)
  (let ((done (make-hash-table :test 'eq))
        (types (loop :for type :being :the :hash-keys :of (lowered-types lowered) :collect type)))
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

(defun float-literal (value bits)
  (cond ((float-features:float-nan-p value) "NAN")
        ((float-features:float-infinity-p value) (if (plusp value) "INFINITY" "(-INFINITY)"))
        (t (multiple-value-bind (mantissa exponent sign) (integer-decode-float value)
             (format nil "~:[~;-~]0x~xp~d~:[~;f~]" (minusp sign) mantissa exponent (= bits 32))))))

(defun c-literal (value type)
  (let* ((name (ir-type-name type))
         (bits (numeric-bits name)))
    (case (numeric-kind name)
      (:wrap (when (typep value `(unsigned-byte ,bits))
               (return-from c-literal (format nil "UINT~d_C(~d)" bits value))))
      (:trap (when (typep value `(signed-byte ,bits))
               (return-from c-literal
                 (if (= value (- (expt 2 63)))
                     "(-INT64_C(9223372036854775807) - 1)"
                     (format nil "INT~d_C(~d)" bits value)))))
      (:float (when (floatp value)
                (return-from c-literal (float-literal value bits)))))
    (emit-error "the literal ~s of type ~a" value (type-text type))))

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
;; https://github.com/takeiteasy/sheaf/issues/13
(defun member-access (access type member)
  (format nil (if (pooled-p type) "(~a)->~a" "(~a).~a") access member))

(defun tagged-literal (type tag args)
  (format nil "(~a){ .tag = ~d~@[, .u.c~d = { ~{~a~^, ~} }~] }" (c-type type) tag (and args tag) args))

(defun constructor-literal (type name &rest args)
  (tagged-literal type (ir-constructor-tag (named-constructor *lowered* type name)) args))

(defun element-access (type expression)
  "The C expressions for the elements and the length of EXPRESSION, a value of
the array, growable or slice TYPE."
  (ecase (storage-layout type)
    ((:array :grow) (values (format nil "(~a)->e" expression) (format nil "(~a)->length" expression)))
    (:slice (values (format nil "(~a).e" expression) (format nil "(~a).length" expression)))))

(defun push-array (node length)
  "Write the statements that push the array NODE builds with LENGTH elements,
and return its C variable."
  (let* ((type (ir-node-type node))
         (array (fresh-c-name (make-symbol "ARRAY")))
         (struct (format nil "struct ~a" (array-struct :array type))))
    (line "~a ~a = sheaf_push(&sheaf_stacks[~d], offsetof(~a, e) + ~a * sizeof(~a), _Alignof(~a));"
          (c-type type) array (ir-prim-stack node) struct length (c-type (element-type type)) struct)
    (line "~a->length = ~a;" array length)
    array))

(defun prelude-expr (node env)
  "A C expression for the prelude operation NODE, after writing any statements it needs."
  (let* ((op (ir-prim-op node))
         (args (mapcar (lambda (arg) (atom-expr arg env)) (ir-prim-args node)))
         (arg-types (mapcar #'ir-node-type (ir-prim-args node)))
         (type (ir-node-type node)))
    (labels ((unit () (constructor-literal type "UNIT"))
             (element-c (storage-type) (c-type (element-type storage-type)))
             (stack ()
               (ir-prim-stack node))
             (new-array (length value)
               (let ((array (push-array node length))
                     (index (fresh-c-name (make-symbol "I")))
                     (fill (fresh-c-name (make-symbol "FILL"))))
                 (line "~a ~a = ~a;" (element-c type) fill value)
                 (line "for (uint64_t ~a = 0; ~a < ~a->length; ~a++) ~a->e[~a] = ~a;"
                       index index array index array index fill)
                 array))
             (growable-stack ()
               (if (ir-prim-bank node)
                   (let ((slice (fresh-c-name (make-symbol "SLICE"))))
                     (line "sheaf_stack *~a = sheaf_bank_stack(&sheaf_banks[~d]);" slice (ir-prim-bank node))
                     slice)
                   (format nil "&sheaf_stacks[~d]" (stack))))
             (new-table (hint)
               (let ((table (fresh-c-name (make-symbol "TABLE")))
                     (struct (format nil "struct ~a" (array-struct :table type))))
                 (line "~a ~a = (~a)sheaf_table_new(~a, offsetof(~a, e), sizeof(((~a *)0)->e[0]), _Alignof(~a), ~a);"
                       (c-type type) table (c-type type) (growable-stack) struct struct struct hint)
                 table))
             (table-slots (table)
               (format nil "(char *)(~a)->e" table))
             (table-slot (table)
               (format nil "sizeof((~a)->e[0])" table))
             (new-growable ()
               (let ((growable (fresh-c-name (make-symbol "GROW")))
                     (on (growable-stack)))
                 (line "~a ~a = sheaf_push(~a, offsetof(struct ~a, e), _Alignof(struct ~a));"
                       (c-type type) growable on (array-struct :grow type) (array-struct :grow type))
                 (line "~a->stack = ~a;" growable on)
                 (line "~a->length = 0;" growable)
                 growable))
             (push-element (growable value storage-type)
               (let ((index (fresh-c-name (make-symbol "INDEX")))
                     (slot (fresh-c-name (make-symbol "SLOT")))
                     (elem (element-c storage-type)))
                 (line "uint64_t ~a = (~a)->length;" index growable)
                 (line "~a *~a = sheaf_grow((~a)->stack, (~a)->e + ~a, sizeof(~a));"
                       elem slot growable growable index elem)
                 (line "*~a = ~a;" slot value)
                 (line "(~a)->length = ~a + 1;" growable index)
                 index))
             (pop-element (growable)
               (let ((result (fresh-c-name (make-symbol "POPPED"))))
                 (line "~a ~a;" (c-type type) result)
                 (line "if ((~a)->length == 0) {" growable)
                 (indented (line "~a = ~a;" result (constructor-literal type "NONE")))
                 (line "} else {")
                 (indented
                   (line "(~a)->length--;" growable)
                   (line "sheaf_shrink((~a)->stack, (~a)->e + (~a)->length + 1, (~a)->e + (~a)->length);"
                         growable growable growable growable growable)
                   (line "~a = ~a;" result
                         (constructor-literal type "SOME" (format nil "(~a)->e[(~a)->length]" growable growable))))
                 (line "}")
                 result))
             (checked (array-type array index)
               (multiple-value-bind (data length) (element-access array-type array)
                 (format nil "~a[sheaf_index(~a, ~a)]" data index length))))
      (destructuring-bind (&optional a b c) args
        (destructuring-bind (&optional type-a &rest others) arg-types
          (declare (ignore others))
          (ecase op
            (:fixed-new (new-array (format nil "UINT64_C(~d)" (fixed-length type)) a))
            (:array-new
             (let ((length (fresh-c-name (make-symbol "LENGTH"))))
               (line "uint64_t ~a = ~a;" length a)
               (new-array length b)))
            (:cell-new
             (let ((cell (fresh-c-name (make-symbol "CELL"))))
               (line "~a ~a = sheaf_push(&sheaf_stacks[~d], sizeof(~a), _Alignof(~a));"
                     (c-type type) cell (stack) (element-c type) (element-c type))
               (line "*~a = ~a;" cell a)
               cell))
            ((:grow-new :stack-new :vector-new) (new-growable))
            ((:grow-push :stack-push)
             (push-element a b type-a)
             (unit))
            (:vector-push (push-element b a (second arg-types)))
            ((:stack-pop :vector-pop) (pop-element a))
            (:vector-index
             (multiple-value-bind (data length) (element-access (second arg-types) b)
               (format nil "(~a < ~a ? ~a : ~a)" a length
                       (constructor-literal type "SOME" (format nil "~a[~a]" data a))
                       (constructor-literal type "NONE"))))
            (:vector-index-unsafe (checked (second arg-types) b a))
            (:vector-set
             (line "~a = ~a;" (checked (third arg-types) c a) b)
             (unit))
            (:vector-length (nth-value 1 (element-access type-a a)))
            (:elements-length (nth-value 1 (element-access type-a a)))
            (:elements-index (checked type-a a b))
            (:elements-set
             (line "~a = ~a;" (checked type-a a b) c)
             (unit))
            (:elements-ref (format nil "&~a" (checked type-a a b)))
            (:elements-slice
             (multiple-value-bind (data length) (element-access type-a a)
               (line "sheaf_range(~a, ~a, ~a);" b c length)
               (format nil "(~a){ ~a + ~a, ~a - ~a }" (c-type type) data b c b)))
            (:ref-get (format nil "(*~a)" a))
            (:ref-set
             (line "*~a = ~a;" a b)
             (unit))
            (:cell-read (format nil "(*~a)" a))
            (:cell-write
             (line "*~a = ~a;" a b)
             b)
            (:cell-swap
             (let ((old (fresh-c-name (make-symbol "OLD"))))
               (line "~a ~a = *~a;" (element-c type-a) old a)
               (line "*~a = ~a;" a b)
               old))
            (:hashtable-new (new-table 0))
            (:hashtable-with-capacity (new-table (format nil "(~a < 0 ? UINT64_C(0) : (uint64_t)~a)" a a)))
            (:hashtable-get
             (let ((found (fresh-c-name (make-symbol "FOUND"))))
               (line "uint64_t ~a = sheaf_table_find(&(~a)->table, ~a, ~a, ~a);"
                     found a (table-slots a) (table-slot a) b)
               (format nil "(~a == (~a)->table.capacity ? ~a : ~a)" found a
                       (constructor-literal type "NONE")
                       (constructor-literal type "SOME" (format nil "(~a)->e[~a].value" a found)))))
            (:hashtable-set
             (let ((slot (fresh-c-name (make-symbol "SLOT"))))
               (line "uint64_t ~a = sheaf_table_insert(&(~a)->table, ~a, ~a, ~a);"
                     slot a (table-slots a) (table-slot a) b)
               (line "(~a)->e[~a].value = ~a;" a slot c)
               (unit)))
            (:hashtable-remove
             (line "sheaf_table_remove(&(~a)->table, ~a, ~a, ~a);" a (table-slots a) (table-slot a) b)
             (unit))
            (:hashtable-count (format nil "(int64_t)(~a)->table.count" a))))))))

(defparameter *arithmetic* '((:add "+" "add") (:sub "-" "sub") (:mul "*" "mul")))

(defun arithmetic-expr (op name a b)
  (destructuring-bind (operator function) (rest (assoc op *arithmetic*))
    (let ((bits (numeric-bits name))
          (c-type (numeric-c-type name)))
      (ecase (numeric-kind name)
        (:wrap (if (= bits 64)
                   (format nil "(~a ~a ~a)" a operator b)
                   (format nil "(~a)((uint64_t)~a ~a (uint64_t)~a)" c-type a operator b)))
        (:trap (format nil "sheaf_~a_i~d(~a, ~a)" function bits a b))
        (:float (format nil "(~a)(~a ~a ~a)" c-type a operator b))))))

(defun conversion-expr (name a)
  (let ((bits (numeric-bits name))
        (c-type (numeric-c-type name)))
    (ecase (numeric-kind name)
      ((:wrap :float) (format nil "(~a)~a" c-type a))
      (:trap (if (= bits 64) a (format nil "sheaf_fit_i~d(~a)" bits a))))))

(defun prim-expr (node env)
  (when (prelude-op-p (ir-prim-op node))
    (return-from prim-expr (prelude-expr node env)))
  (destructuring-bind (&optional a b) (mapcar (lambda (arg) (atom-expr arg env)) (ir-prim-args node))
    (let* ((type (ir-node-type node))
           (name (ir-type-name type)))
      (flet ((tag (constructor) (ir-constructor-tag (named-constructor *lowered* type constructor)))
             (boolean (test)
               (format nil "(~a){ .tag = ~a ? ~d : ~d }" (c-type type) test
                       (ir-constructor-tag (named-constructor *lowered* type "TRUE"))
                       (ir-constructor-tag (named-constructor *lowered* type "FALSE")))))
        (ecase (ir-prim-op node)
          ((:add :sub :mul) (arithmetic-expr (ir-prim-op node) name a b))
          (:from-int (conversion-expr name a))
          (:eq (boolean (if (numeric-entry (ir-type-name (ir-node-type (first (ir-prim-args node)))))
                            (format nil "~a == ~a" a b)
                            (format nil "~a.tag == ~a.tag" a b))))
          (:compare (format nil "(~a){ .tag = ~a < ~a ? ~d : ~a > ~a ? ~d : ~d }"
                            (c-type type) a b (tag "LT") a b (tag "GT") (tag "EQ"))))))))

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
        (tagged-literal type tag args))))

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
    (ir-foreign (emit-error "raw Lisp: https://github.com/takeiteasy/sheaf/issues/7"))))

;;; Statements

(defun emit-releases (releases)
  (loop :for (kind index mark) :in releases
        :do (ecase kind
              (:stack (line "sheaf_stacks[~d].top = ~a;" index mark))
              (:bank (line "sheaf_bank_release(&sheaf_banks[~d], ~a);" index mark)))))

(defun emit-return (expression)
  (if *releases*
      (let ((result (fresh-c-name (make-symbol "RESULT"))))
        (line "~a ~a = ~a;" *result-c-type* result expression)
        (emit-releases *releases*)
        (line "return ~a;" result))
      (line "return ~a;" expression)))

(defun self-tail-call-p (node)
  (and (typep node 'ir-call)
       (eq (ir-call-function node) *self*)
       (null (ir-call-region node))
       (null *releases*)))

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
     (let* ((name (fresh-c-name (ir-let-name node)))
            (bank (ir-let-bank node))
            (owned (append (loop :for stack :in (ir-let-stacks node)
                                 :collect (list :stack stack (fresh-c-name (make-symbol "MARK"))))
                           (and bank (list (list :bank bank (fresh-c-name (make-symbol "MARK"))))))))
       (loop :for (kind index mark) :in owned
             :do (ecase kind
                   (:stack (line "char *~a = sheaf_stacks[~d].top;" mark index))
                   (:bank (line "sheaf_slice *~a = sheaf_banks[~d].head;" mark index))))
       (line "~a ~a SHEAF_UNUSED;" (c-type (ir-node-type (ir-let-value node))) name)
       (emit-node (ir-let-value node) name env)
       (let ((*releases* (append owned *releases*)))
         (emit-node (ir-let-body node) target (acons (ir-let-name node) name env)))
       (unless (eq target :return)
         (emit-releases owned))))
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
         (*releases* (and (ir-def-pool def) (list (list :stack (ir-def-pool def) "pool_mark"))))
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
                   (when *releases*
                     (line "char *pool_mark = sheaf_stacks[~d].top;" (ir-def-pool def)))
                   (cond (value-p
                          (emit-node (ir-def-body def) "value" env)
                          (emit-releases *releases*)
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
      (case (numeric-kind (ir-type-name (ir-def-result root)))
        (:wrap (line "printf(\"%llu\\n\", (unsigned long long)~a);" call))
        (:trap (line "printf(\"%lld\\n\", (long long)~a);" call))
        (:float (line "printf(\"%.17g\\n\", (double)~a);" call))
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
            (*c-arrays* (make-hash-table :test 'equal))
            (*c-names* (make-hash-table :test 'eq))
            (*c-counter* 0)
            (*emit-def* nil))
        (maphash (lambda (name def)
                   (declare (ignore def))
                   (setf (gethash name *c-names*) (fresh-c-name name)))
                 (lowered-defs assigned))
        (maphash (lambda (type definition)
                   (declare (ignore definition))
                   (struct-name type))
                 (lowered-types assigned))
        (let* ((body (with-output-to-string (*out*)
                       (line "static sheaf_stack sheaf_stacks[~d];" (max 1 (lowered-stack-count assigned)))
                       (line "static sheaf_bank sheaf_banks[~d] SHEAF_UNUSED;" (max 1 (lowered-stack-count assigned)))
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
                       (emit-main assigned)))
               (types (with-output-to-string (*out*)
                        (emit-type-definitions assigned))))
          (line "// Generated by Sheaf.")
          (line "#include \"sheaf.h\"")
          (terpri *out*)
          (emit-forward-declarations)
          (terpri *out*)
          (emit-array-definitions '(:slice))
          (write-string types *out*)
          (emit-array-definitions '(:array :grow :table))
          (terpri *out*)
          (write-string body *out*)))
      (with-output-to-string (stream)
        (emit-c assigned stream))))

(defun write-c (assigned pathname)
  "Write ASSIGNED to the C file PATHNAME. Returns PATHNAME."
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (emit-c assigned stream))
  pathname)
