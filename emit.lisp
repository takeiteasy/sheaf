;;;; emit.lisp
;;;; Emitting an assigned program as one C file that includes runtime/sheaf.h.

(in-package #:sheaf)

(defvar *statements*)
(defvar *c-types*)
(defvar *c-arrays*)
(defvar *c-names*)
(defvar *c-counter*)
(defvar *emit-def* nil "The def being emitted, for errors.")
(defvar *releases* '() "What to give back on return, each `:stack` or `:bank`, its index and the C variable holding its mark.")
(defvar *result-c-type*)
(defvar *tail-used*)
(defvar *jumps* '() "Where a tail call goes: the callee's name, its label and its parameters, each a C type and name.")
(defvar *tail-calls* '() "The calls in tail position of the def being emitted that jump.")
(defvar *def-tail-calls*)
(defvar *def-cycle*)

(defun emit-error (control &rest args)
  (let ((location (and *emit-def* (ir-def-location *emit-def*))))
    (error "The C emitter does not support ~?~@[ in ~a~]~@[, at offset ~d~]"
           control args
           (and *emit-def* (or (ir-def-origin *emit-def*) (ir-def-name *emit-def*)))
           (and location (source:span-start (source:location-span location))))))

;;; Statements a helper needs first

(defmacro with-statements (&body body)
  "Run BODY, which calls `emit-stmt` for each statement it needs first, in
order. Returns BODY's value, an expression, and the statements."
  `(let ((*statements* '()))
     (values (progn ,@body) (nreverse *statements*))))

(defun emit-stmt (statement)
  (push statement *statements*))

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
  "The C type of TYPE, as a node."
  (ecase (ir-type-kind type)
    (:scalar (let ((c-type (numeric-c-type (ir-type-name type))))
               (if c-type
                   `(:name ,c-type)
                   (emit-error "the type ~a: https://github.com/takeiteasy/sheaf/issues/22" (type-text type)))))
    ((:storage :view)
     (let ((layout (storage-layout type)))
       (case layout
         ((:array :grow :table) `(:ptr (:struct ,(array-struct layout type))))
         (:slice `(:struct ,(array-struct layout type)))
         (:pointer `(:ptr ,(c-type (element-type type)))))))
    (:data (if (pooled-p type)
               `(:ptr (:struct ,(struct-name type)))
               `(:struct ,(struct-name type))))))

(defun forward-declarations ()
  (append (loop :for id :being :the :hash-values :of *c-types*
                :collect `(:struct-decl ,(format nil "T~d" id)))
          (loop :for (layout) :being :the :hash-keys :of *c-arrays* :using (:hash-value id)
                :collect `(:struct-decl ,(format nil "~a~d" (array-prefix layout) id)))))

(defun array-prefix (layout)
  (ecase layout (:array "A") (:grow "G") (:table "H") (:slice "S")))

(defun array-definitions (layouts)
  "The C structs of the array layouts LAYOUTS, in the order they were first used."
  (let ((entries '())
        (u64 '(:name "uint64_t")))
    (maphash (lambda (key id) (when (member (first key) layouts) (push (cons id key) entries))) *c-arrays*)
    (loop :for (id layout element) :in (sort entries #'< :key #'car)
          :collect (let ((name (format nil "~a~d" (array-prefix layout) id)))
                     (ecase layout
                       (:slice `(:struct-def ,name (:field (:ptr ,element) "e") (:field ,u64 "length")))
                       (:array `(:struct-def ,name (:field ,u64 "length") (:flexible ,element "e")))
                       (:grow `(:struct-def ,name (:field (:ptr (:name "sheaf_stack")) "stack")
                                            (:field ,u64 "length") (:flexible ,element "e")))
                       (:table `(:struct-def ,name (:field (:name "sheaf_table") "table")
                                             (:anon-struct "e" t (:field ,u64 "key") (:field ,u64 "used")
                                                           (:field ,element "value")))))))))

(defun type-definitions (lowered)
  "The C structs of the data types of LOWERED, each after the structs it holds by value."
  (let ((done (make-hash-table :test 'eq))
        (types (loop :for type :being :the :hash-keys :of (lowered-types lowered) :collect type))
        (nodes '()))
    (labels ((define (type)
               (unless (gethash type done)
                 (setf (gethash type done) t)
                 (dolist (field (field-types type))
                   (when (and (eq (ir-type-kind field) :data) (not (pooled-p field)))
                     (define field)))
                 (let ((constructors (lowered-constructors lowered type)))
                   (push `(:comment ,(type-text type)) nodes)
                   (push `(:struct-def ,(struct-name type)
                                       (:field (:name "uint32_t") "tag")
                                       ,@(when (some #'ir-constructor-fields constructors)
                                           `((:union "u"
                                                     ,@(loop :for constructor :in constructors
                                                             :when (ir-constructor-fields constructor)
                                                               :collect `(:anon-struct
                                                                          ,(format nil "c~d" (ir-constructor-tag constructor))
                                                                          nil
                                                                          ,@(loop :for field :in (ir-constructor-fields constructor)
                                                                                  :for i :from 0
                                                                                  :collect `(:field ,(c-type field)
                                                                                                    ,(format nil "f~d" i)))))))))
                         nodes)))))
      (mapc #'define types))
    (nreverse nodes)))

;;; Expressions

(defun var (name)
  `(:name ,name))

(defun stack-node (index)
  `(:index (:name "sheaf_stacks") (:int ,index)))

(defun bank-node (index)
  `(:index (:name "sheaf_banks") (:int ,index)))

(defun c-literal (value type)
  (let* ((name (ir-type-name type))
         (bits (numeric-bits name)))
    (case (numeric-kind name)
      (:wrap (when (typep value `(unsigned-byte ,bits))
               (return-from c-literal `(:call ,(format nil "UINT~d_C" bits) (:int ,value)))))
      (:trap (when (typep value `(signed-byte ,bits))
               (return-from c-literal
                 (if (= value (- (expt 2 63)))
                     '(:- (:neg (:call "INT64_C" (:int 9223372036854775807))) (:int 1))
                     `(:call ,(format nil "INT~d_C" bits) (:int ,value))))))
      (:float (when (floatp value)
                (return-from c-literal `(:float ,value)))))
    (emit-error "the literal ~s of type ~a" value (type-text type))))

(defun atom-expr (node env)
  (etypecase node
    (ir-literal (c-literal (ir-literal-value node) (ir-node-type node)))
    (ir-local (var (or (cdr (assoc (ir-local-name node) env))
                       (error "~s is not bound" (ir-local-name node)))))
    (ir-global `(:call ,(c-name (ir-global-name node))))))

(defun constructor-of (name type)
  (find name (lowered-constructors *lowered* type) :key #'ir-constructor-name))

;; TODO: the cells of a recursive type are linked by C pointers, where
;; docs/MEMORY.md describes relative references.
;; https://github.com/takeiteasy/sheaf/issues/13
(defun member-access (access type &rest members)
  "The member path MEMBERS of ACCESS, a value of TYPE."
  (let ((node `(,(if (pooled-p type) :-> :dot) ,access ,(first members))))
    (dolist (member (rest members) node)
      (setf node `(:dot ,node ,member)))))

(defun tagged-literal (type tag args)
  `(:compound ,(c-type type) (".tag" (:int ,tag))
              ,@(when args `((,(format nil ".u.c~d" tag) (:init ,@args))))))

(defun constructor-literal (type name &rest args)
  (tagged-literal type (ir-constructor-tag (named-constructor *lowered* type name)) args))

(defun element-access (type expression)
  "The C expressions for the elements and the length of EXPRESSION, a value of
the array, growable or slice TYPE."
  (ecase (storage-layout type)
    ((:array :grow) (values `(:-> ,expression "e") `(:-> ,expression "length")))
    (:slice (values `(:dot ,expression "e") `(:dot ,expression "length")))))

(defun unit-literal (type)
  (constructor-literal type "UNIT"))

(defun element-c (storage-type)
  (c-type (element-type storage-type)))

(defun new-array (node length value)
  "Push the array NODE builds with LENGTH elements, each VALUE. Returns its C variable."
  (let* ((type (ir-node-type node))
         (array (fresh-c-name (make-symbol "ARRAY")))
         (index (fresh-c-name (make-symbol "I")))
         (fill (fresh-c-name (make-symbol "FILL")))
         (struct `(:struct ,(array-struct :array type))))
    (emit-stmt `(:decl ,(c-type type) ,array
                       (:call "sheaf_push" (:addr ,(stack-node (ir-prim-stack node)))
                              (:+ (:offsetof ,struct "e") (:* ,length (:sizeof ,(element-c type))))
                              (:alignof ,struct))))
    (emit-stmt `(:assign (:-> ,(var array) "length") ,length))
    (emit-stmt `(:decl ,(element-c type) ,fill ,value))
    (emit-stmt `(:for (:decl (:name "uint64_t") ,index (:int 0))
                      (:< ,(var index) (:-> ,(var array) "length"))
                      (:post++ ,(var index))
                      (:assign (:index (:-> ,(var array) "e") ,(var index)) ,(var fill))))
    (var array)))

(defun growable-stack (node)
  "The stack the growable NODE builds grows on, as a C expression."
  (if (ir-prim-bank node)
      (let ((slice (fresh-c-name (make-symbol "SLICE"))))
        (emit-stmt `(:decl (:ptr (:name "sheaf_stack")) ,slice
                           (:call "sheaf_bank_stack" (:addr ,(bank-node (ir-prim-bank node))))))
        (var slice))
      `(:addr ,(stack-node (ir-prim-stack node)))))

(defun new-table (node hint)
  "Push the hash table NODE builds with room for HINT entries. Returns its C variable."
  (let* ((type (ir-node-type node))
         (table (fresh-c-name (make-symbol "TABLE")))
         (struct `(:struct ,(array-struct :table type)))
         (slot `(:index (:-> (:cast (:ptr ,struct) (:int 0)) "e") (:int 0))))
    (emit-stmt `(:decl ,(c-type type) ,table
                       (:cast ,(c-type type)
                              (:call "sheaf_table_new" ,(growable-stack node) (:offsetof ,struct "e")
                                     (:sizeof ,slot) (:alignof ,struct) ,hint))))
    (var table)))

(defun table-slots (table)
  `(:cast (:ptr (:name "char")) (:-> ,table "e")))

(defun table-slot (table)
  `(:sizeof (:index (:-> ,table "e") (:int 0))))

(defun new-growable (node)
  "Push the empty growable NODE builds. Returns its C variable."
  (let* ((type (ir-node-type node))
         (growable (fresh-c-name (make-symbol "GROW")))
         (on (growable-stack node))
         (struct `(:struct ,(array-struct :grow type))))
    (emit-stmt `(:decl ,(c-type type) ,growable
                       (:call "sheaf_push" ,on (:offsetof ,struct "e") (:alignof ,struct))))
    (emit-stmt `(:assign (:-> ,(var growable) "stack") ,on))
    (emit-stmt `(:assign (:-> ,(var growable) "length") (:int 0)))
    (var growable)))

(defun push-element (growable value storage-type)
  "Push VALUE onto GROWABLE, a value of STORAGE-TYPE. Returns the C variable holding its index."
  (let ((index (fresh-c-name (make-symbol "INDEX")))
        (slot (fresh-c-name (make-symbol "SLOT")))
        (elem (element-c storage-type)))
    (emit-stmt `(:decl (:name "uint64_t") ,index (:-> ,growable "length")))
    (emit-stmt `(:decl (:ptr ,elem) ,slot
                       (:call "sheaf_grow" (:-> ,growable "stack")
                              (:+ (:-> ,growable "e") ,(var index)) (:sizeof ,elem))))
    (emit-stmt `(:assign (:deref ,(var slot)) ,value))
    (emit-stmt `(:assign (:-> ,growable "length") (:+ ,(var index) (:int 1))))
    (var index)))

(defun pop-element (growable type)
  "Pop GROWABLE into an `Optional` of TYPE. Returns its C variable."
  (let ((result (fresh-c-name (make-symbol "POPPED")))
        (length `(:-> ,growable "length")))
    (emit-stmt `(:decl ,(c-type type) ,result nil))
    (emit-stmt `(:if (:== ,length (:int 0))
                     ((:assign ,(var result) ,(constructor-literal type "NONE")))
                     ((:expr (:post-- ,length))
                      (:expr (:call "sheaf_shrink" (:-> ,growable "stack")
                                    (:+ (:+ (:-> ,growable "e") ,length) (:int 1))
                                    (:+ (:-> ,growable "e") ,length)))
                      (:assign ,(var result)
                               ,(constructor-literal type "SOME"
                                                     `(:index (:-> ,growable "e") ,length))))))
    (var result)))

(defun checked (array-type array index)
  "The element of ARRAY at INDEX, which aborts if INDEX is past the end."
  (multiple-value-bind (data length) (element-access array-type array)
    `(:index ,data (:call "sheaf_index" ,index ,length))))

(defvar *prelude-emitters* (make-hash-table :test 'eq)
  "The function that builds the C expression of each prelude operation.")

(defmacro define-prelude-op (ops &body body)
  "Define the C expression of the prelude operations OPS, a keyword or a list of
them. BODY runs with NODE, its result TYPE, ARG-TYPES and its operands A, B and
C as C expressions. It calls `emit-stmt` for the statements it needs first and
returns the expression."
  `(let ((emitter (lambda (node type arg-types a b c)
                    (declare (ignorable node type arg-types a b c))
                    ,@body)))
     (dolist (op ',(if (listp ops) ops (list ops)))
       (setf (gethash op *prelude-emitters*) emitter))))

(define-prelude-op :fixed-new
  (new-array node `(:call "UINT64_C" (:int ,(fixed-length type))) a))

(define-prelude-op :array-new
  (let ((length (fresh-c-name (make-symbol "LENGTH"))))
    (emit-stmt `(:decl (:name "uint64_t") ,length ,a))
    (new-array node (var length) b)))

(define-prelude-op :cell-new
  (let ((cell (fresh-c-name (make-symbol "CELL"))))
    (emit-stmt `(:decl ,(c-type type) ,cell
                       (:call "sheaf_push" (:addr ,(stack-node (ir-prim-stack node)))
                              (:sizeof ,(element-c type)) (:alignof ,(element-c type)))))
    (emit-stmt `(:assign (:deref ,(var cell)) ,a))
    (var cell)))

(define-prelude-op (:grow-new :stack-new :vector-new)
  (new-growable node))

(define-prelude-op (:grow-push :stack-push)
  (push-element a b (first arg-types))
  (unit-literal type))

(define-prelude-op :vector-push
  (push-element b a (second arg-types)))

(define-prelude-op (:stack-pop :vector-pop)
  (pop-element a type))

(define-prelude-op :vector-index
  (multiple-value-bind (data length) (element-access (second arg-types) b)
    `(:? (:< ,a ,length)
         ,(constructor-literal type "SOME" `(:index ,data ,a))
         ,(constructor-literal type "NONE"))))

(define-prelude-op :vector-index-unsafe
  (checked (second arg-types) b a))

(define-prelude-op :vector-set
  (emit-stmt `(:assign ,(checked (third arg-types) c a) ,b))
  (unit-literal type))

(define-prelude-op (:vector-length :elements-length)
  (nth-value 1 (element-access (first arg-types) a)))

(define-prelude-op :elements-index
  (checked (first arg-types) a b))

(define-prelude-op :elements-set
  (emit-stmt `(:assign ,(checked (first arg-types) a b) ,c))
  (unit-literal type))

(define-prelude-op :elements-ref
  `(:addr ,(checked (first arg-types) a b)))

(define-prelude-op :elements-slice
  (multiple-value-bind (data length) (element-access (first arg-types) a)
    (emit-stmt `(:expr (:call "sheaf_range" ,b ,c ,length)))
    `(:compound ,(c-type type) (nil (:+ ,data ,b)) (nil (:- ,c ,b)))))

(define-prelude-op (:ref-get :cell-read)
  `(:deref ,a))

(define-prelude-op :ref-set
  (emit-stmt `(:assign (:deref ,a) ,b))
  (unit-literal type))

(define-prelude-op :cell-write
  (emit-stmt `(:assign (:deref ,a) ,b))
  b)

(define-prelude-op :cell-swap
  (let ((old (fresh-c-name (make-symbol "OLD"))))
    (emit-stmt `(:decl ,(element-c (first arg-types)) ,old (:deref ,a)))
    (emit-stmt `(:assign (:deref ,a) ,b))
    (var old)))

(define-prelude-op :hashtable-new
  (new-table node '(:int 0)))

(define-prelude-op :hashtable-with-capacity
  (new-table node `(:? (:< ,a (:int 0)) (:call "UINT64_C" (:int 0)) (:cast (:name "uint64_t") ,a))))

(define-prelude-op :hashtable-get
  (let ((found (fresh-c-name (make-symbol "FOUND"))))
    (emit-stmt `(:decl (:name "uint64_t") ,found
                       (:call "sheaf_table_find" (:addr (:-> ,a "table")) ,(table-slots a)
                              ,(table-slot a) ,b)))
    `(:? (:== ,(var found) (:dot (:-> ,a "table") "capacity"))
         ,(constructor-literal type "NONE")
         ,(constructor-literal type "SOME" `(:dot (:index (:-> ,a "e") ,(var found)) "value")))))

(define-prelude-op :hashtable-set
  (let ((slot (fresh-c-name (make-symbol "SLOT"))))
    (emit-stmt `(:decl (:name "uint64_t") ,slot
                       (:call "sheaf_table_insert" (:addr (:-> ,a "table")) ,(table-slots a)
                              ,(table-slot a) ,b)))
    (emit-stmt `(:assign (:dot (:index (:-> ,a "e") ,(var slot)) "value") ,c))
    (unit-literal type)))

(define-prelude-op :hashtable-remove
  (emit-stmt `(:expr (:call "sheaf_table_remove" (:addr (:-> ,a "table")) ,(table-slots a)
                            ,(table-slot a) ,b)))
  (unit-literal type))

(define-prelude-op :hashtable-count
  `(:cast (:name "int64_t") (:dot (:-> ,a "table") "count")))

(defun prelude-expr (node env)
  "A C expression for the prelude operation NODE, and the statements it needs first."
  (let ((args (mapcar (lambda (arg) (atom-expr arg env)) (ir-prim-args node))))
    (with-statements
      (destructuring-bind (&optional a b c) args
        (funcall (gethash (ir-prim-op node) *prelude-emitters*)
                 node (ir-node-type node) (mapcar #'ir-node-type (ir-prim-args node)) a b c)))))

(defparameter *arithmetic* '((:add :+ "add") (:sub :- "sub") (:mul :* "mul")))

(defun arithmetic-expr (op name a b)
  (destructuring-bind (operator function) (rest (assoc op *arithmetic*))
    (let ((bits (numeric-bits name))
          (c-type `(:name ,(numeric-c-type name))))
      (ecase (numeric-kind name)
        (:wrap (if (= bits 64)
                   `(,operator ,a ,b)
                   `(:cast ,c-type (,operator (:cast (:name "uint64_t") ,a) (:cast (:name "uint64_t") ,b)))))
        (:trap `(:call ,(format nil "sheaf_~a_i~d" function bits) ,a ,b))
        (:float `(:cast ,c-type (,operator ,a ,b)))))))

(defun conversion-expr (name a)
  (let ((bits (numeric-bits name))
        (c-type `(:name ,(numeric-c-type name))))
    (ecase (numeric-kind name)
      ((:wrap :float) `(:cast ,c-type ,a))
      (:trap (if (= bits 64) a `(:call ,(format nil "sheaf_fit_i~d" bits) ,a))))))

(defun numeric-expr (node env)
  (destructuring-bind (&optional a b) (mapcar (lambda (arg) (atom-expr arg env)) (ir-prim-args node))
    (let* ((type (ir-node-type node))
           (name (ir-type-name type)))
      (flet ((tag (constructor) `(:int ,(ir-constructor-tag (named-constructor *lowered* type constructor))))
             (tagged (test)
               `(:compound ,(c-type type) (".tag" ,test))))
        (ecase (ir-prim-op node)
          ((:add :sub :mul) (arithmetic-expr (ir-prim-op node) name a b))
          (:from-int (conversion-expr name a))
          (:eq (tagged `(:? ,(if (numeric-entry (ir-type-name (ir-node-type (first (ir-prim-args node)))))
                                 `(:== ,a ,b)
                                 `(:== (:dot ,a "tag") (:dot ,b "tag")))
                            ,(tag "TRUE") ,(tag "FALSE"))))
          (:compare (tagged `(:? (:< ,a ,b) ,(tag "LT") (:? (:> ,a ,b) ,(tag "GT") ,(tag "EQ"))))))))))

(defun prim-expr (node env)
  "A C expression for the primitive NODE, and the statements it needs first."
  (if (prelude-op-p (ir-prim-op node))
      (prelude-expr node env)
      (values (numeric-expr node env) '())))

(defun construct-expr (node env)
  "A C expression for the construction NODE, and the statements it needs first."
  (let* ((type (ir-node-type node))
         (constructor (constructor-of (ir-construct-constructor node) type))
         (tag (ir-constructor-tag constructor))
         (args (mapcar (lambda (arg) (atom-expr arg env)) (ir-construct-args node))))
    (if (pooled-p type)
        (let ((cell (fresh-c-name (make-symbol "CELL")))
              (struct `(:struct ,(struct-name type))))
          (values (var cell)
                  (list* `(:decl (:ptr ,struct) ,cell
                                 (:call "sheaf_push" (:addr ,(stack-node (ir-construct-stack node)))
                                        (:sizeof ,struct) (:alignof ,struct)))
                         `(:assign (:-> ,(var cell) "tag") (:int ,tag))
                         (loop :for arg :in args
                               :for i :from 0
                               :collect `(:assign ,(member-access (var cell) type "u" (format nil "c~d" tag)
                                                                  (format nil "f~d" i))
                                                  ,arg)))))
        (values (tagged-literal type tag args) '()))))

(defun call-expr (node env)
  "A C expression for the call NODE, and the statements it needs first."
  (let ((call `(:call ,(c-name (ir-call-function node))
                      ,@(mapcar (lambda (arg) (atom-expr arg env)) (ir-call-args node))))
        (region (ir-call-region node)))
    (if region
        (let ((mark (fresh-c-name (make-symbol "MARK")))
              (result (fresh-c-name (make-symbol "RESULT"))))
          (values (var result)
                  (list `(:decl (:ptr (:name "char")) ,mark (:dot ,(stack-node region) "top"))
                        `(:decl ,(c-type (ir-node-type node)) ,result ,call)
                        `(:assign (:dot ,(stack-node region) "top") ,(var mark)))))
        (values call '()))))

(defun simple-expr (node env)
  "A C expression for the simple NODE, and the statements it needs first."
  (etypecase node
    (ir-atom (values (atom-expr node env) '()))
    (ir-prim (prim-expr node env))
    (ir-construct (construct-expr node env))
    (ir-call (call-expr node env))
    (ir-foreign (emit-error "raw Lisp: https://github.com/takeiteasy/sheaf/issues/7"))))

;;; Statements

(defun emit-releases (releases)
  "The statements that give RELEASES back."
  (loop :for (kind index mark) :in releases
        :collect (ecase kind
                   (:stack `(:assign (:dot ,(stack-node index) "top") ,(var mark)))
                   (:bank `(:expr (:call "sheaf_bank_release" (:addr ,(bank-node index)) ,(var mark)))))))

(defun emit-return (expression)
  (if *releases*
      (let ((result (fresh-c-name (make-symbol "RESULT"))))
        `((:decl ,*result-c-type* ,result ,expression)
          ,@(emit-releases *releases*)
          (:return ,(var result))))
      `((:return ,expression))))

(defun tail-calls (def)
  "The calls in tail position of DEF that nothing needs to be given back after,
which `emit-node` can turn into jumps."
  (let ((calls '()))
    (labels ((walk (node owned)
               (etypecase node
                 (ir-let (walk (ir-let-body node) (or owned (ir-let-stacks node) (ir-let-bank node))))
                 (ir-match (dolist (branch (ir-match-branches node))
                             (walk (ir-branch-body branch) owned)))
                 (ir-call (unless (or owned (ir-call-region node))
                            (push node calls)))
                 ((or ir-atom ir-prim ir-construct ir-foreign)))))
      (when (eq (ir-def-kind def) :function)
        (walk (ir-def-body def) (ir-def-pool def))))
    calls))

(defun find-tail-cycles (defs)
  "Fill `*def-tail-calls*` with the calls of each def in DEFS that jump to a def
in its own cycle of tail calls, and `*def-cycle*` with each def's cycle, the
names of the defs that tail call each other."
  (let ((calls (make-hash-table :test 'eq)))
    (maphash (lambda (name def) (setf (gethash name calls) (tail-calls def))) defs)
    (dolist (cycle (strongly-connected-components
                    (loop :for name :being :the :hash-keys :of defs :collect name)
                    (lambda (name)
                      (remove-duplicates (mapcar #'ir-call-function (gethash name calls))))))
      (dolist (name cycle)
        (setf (gethash name *def-cycle*) cycle
              (gethash name *def-tail-calls*)
              (remove-if-not (lambda (call) (member (ir-call-function call) cycle))
                             (gethash name calls)))))))

(defun emit-tail-call (node env)
  (setf *tail-used* t)
  (destructuring-bind (label params) (rest (assoc (ir-call-function node) *jumps*))
    (let ((temps (loop :for nil :in params
                       :collect (fresh-c-name (make-symbol "ARG")))))
      `((:block
          ,@(loop :for arg :in (ir-call-args node)
                  :for (type) :in params
                  :for temp :in temps
                  :collect `(:decl ,type ,temp ,(atom-expr arg env)))
          ,@(loop :for (nil name) :in params
                  :for temp :in temps
                  :collect `(:assign ,(var name) ,(var temp)))
          (:goto ,label))))))

(defun pattern-code (pattern access)
  "The C tests that ACCESS matches PATTERN and the variables it binds, each a
symbol, a type and the expression for its value."
  (etypecase pattern
    (ir-pwild (values '() '()))
    (ir-pvar (values '() (list (list (ir-pvar-name pattern) (ir-pattern-type pattern) access))))
    (ir-plit (values (list `(:== ,access ,(c-literal (ir-plit-value pattern) (ir-pattern-type pattern))))
                     '()))
    (ir-pas (multiple-value-bind (tests bindings) (pattern-code (ir-pas-pattern pattern) access)
              (values tests (cons (list (ir-pas-name pattern) (ir-pattern-type pattern) access)
                                  bindings))))
    (ir-pcon
     (let* ((type (ir-pattern-type pattern))
            (tag (ir-constructor-tag (constructor-of (ir-pcon-constructor pattern) type)))
            (tests (list `(:== ,(member-access access type "tag") (:int ,tag))))
            (bindings '()))
       (loop :for field :in (ir-pcon-patterns pattern)
             :for i :from 0
             :do (multiple-value-bind (field-tests field-bindings)
                     (pattern-code field (member-access access type "u" (format nil "c~d" tag) (format nil "f~d" i)))
                   (setf tests (append tests field-tests)
                         bindings (append bindings field-bindings))))
       (values tests bindings)))))

(defun emit-match (node target env)
  "The statements that compute the match NODE into TARGET: an `if` chain, after
a copy of the scrutinee if it is a global."
  (let* ((scrutinee (ir-match-scrutinee node))
         (copy (and (typep scrutinee 'ir-global) (fresh-c-name (make-symbol "SCRUTINEE"))))
         (access (if copy (var copy) (atom-expr scrutinee env)))
         (branches (loop :for branch :in (ir-match-branches node)
                         :collect (multiple-value-bind (tests bindings) (pattern-code (ir-branch-pattern branch) access)
                                    (let ((branch-env env)
                                          (declarations '()))
                                      (loop :for (name type expression) :in bindings
                                            :do (let ((c-name (fresh-c-name name)))
                                                  (push `(:decl ,(c-type type) ,c-name ,expression :unused t)
                                                        declarations)
                                                  (push (cons name c-name) branch-env)))
                                      (list (if tests (reduce (lambda (a b) `(:&& ,a ,b)) tests) '(:int 1))
                                            (append (nreverse declarations)
                                                    (emit-node (ir-branch-body branch) target branch-env))))))))
    (append (when copy
              (list `(:decl ,(c-type (ir-node-type scrutinee)) ,copy ,(atom-expr scrutinee env))))
            (let ((chain (list '(:expr (:call "sheaf_no_match")))))
              (loop :for (test then) :in (reverse branches)
                    :do (setf chain (list `(:if ,test ,then ,chain))))
              chain))))

(defun emit-node (node target env)
  "The statements that compute NODE into TARGET, a C variable or `:return`."
  (etypecase node
    (ir-let
     (let* ((name (fresh-c-name (ir-let-name node)))
            (bank (ir-let-bank node))
            (owned (append (loop :for stack :in (ir-let-stacks node)
                                 :collect (list :stack stack (fresh-c-name (make-symbol "MARK"))))
                           (and bank (list (list :bank bank (fresh-c-name (make-symbol "MARK"))))))))
       (append
        (loop :for (kind index mark) :in owned
              :collect (ecase kind
                         (:stack `(:decl (:ptr (:name "char")) ,mark (:dot ,(stack-node index) "top")))
                         (:bank `(:decl (:ptr (:name "sheaf_slice")) ,mark (:dot ,(bank-node index) "head")))))
        `((:decl ,(c-type (ir-node-type (ir-let-value node))) ,name nil :unused t))
        (emit-node (ir-let-value node) name env)
        (let ((*releases* (append owned *releases*)))
          (emit-node (ir-let-body node) target (acons (ir-let-name node) name env)))
        (unless (eq target :return)
          (emit-releases owned)))))
    (ir-match (emit-match node target env))
    ((or ir-atom ir-prim ir-construct ir-call ir-foreign)
     (if (and (eq target :return) (null *releases*) (member node *tail-calls* :test #'eq))
         (emit-tail-call node env)
         (multiple-value-bind (expression statements) (simple-expr node env)
           (append statements
                   (if (eq target :return)
                       (emit-return expression)
                       `((:assign ,(var target) ,expression)))))))))

;;; Definitions

(defun def-params (def)
  "The parameters of DEF as C types and fresh names."
  (loop :for (symbol . type) :in (ir-def-params def)
        :collect (list (c-type type) (fresh-c-name symbol))))

(defun emit-body (def params)
  "The statements of DEF's body once PARAMS, its C parameters, hold the arguments.
A value stores its result in the static `value`."
  (let* ((*emit-def* def)
         (*result-c-type* (c-type (ir-def-result def)))
         (*releases* (and (ir-def-pool def) (list (list :stack (ir-def-pool def) "pool_mark"))))
         (env (loop :for (symbol) :in (ir-def-params def)
                    :for (nil name) :in params
                    :collect (cons symbol name))))
    (append
     (when *releases*
       `((:decl (:ptr (:name "char")) "pool_mark" (:dot ,(stack-node (ir-def-pool def)) "top"))))
     (cond ((eq (ir-def-kind def) :value)
            (append (emit-node (ir-def-body def) "value" env)
                    (emit-releases *releases*)
                    `((:assign ,(var "ready") (:int 1))
                      (:return ,(var "value")))))
           (t (emit-node (ir-def-body def) :return env))))))

(defun emit-def (def)
  "The C nodes of the function DEF: a comment, the function and a blank line."
  (let* ((*emit-def* def)
         (name (ir-def-name def))
         (*tail-used* nil)
         (*tail-calls* (gethash name *def-tail-calls*))
         (params (def-params def))
         (*jumps* (list (list name "top" params)))
         (body (emit-body def params))
         (result (c-type (ir-def-result def))))
    `((:comment ,(symbol-name name))
      (:fn ,result ,(c-name name) ,params
           (,@(when (eq (ir-def-kind def) :value)
                `((:decl ,result "value" nil :static t)
                  (:decl (:name "int") "ready" nil :static t)
                  (:if ,(var "ready") ((:return ,(var "value"))))))
            ,@(when *tail-used* '((:label "top")))
            ,@body))
      (:blank))))

(defun member-field (struct-member index)
  `(:dot (:dot (:dot ,(var "args") "u") ,(format nil "m~d" struct-member)) ,(format nil "p~d" index)))

(defun emit-cycle (defs)
  "The C nodes of DEFS, functions that tail call each other: one function holding
all their bodies, which a tail call between them jumps within, and a function
for each def that enters it."
  (let* ((result (ir-def-result (first defs)))
         (struct (fresh-c-name (make-symbol "ARGS")))
         (function (fresh-c-name (make-symbol "CYCLE")))
         (members (loop :for def :in defs
                        :collect (let ((*emit-def* def))
                                   (list def (def-params def) (fresh-c-name (make-symbol "ENTER"))))))
         (jumps (loop :for (def params label) :in members
                      :collect (list (ir-def-name def) label params)))
         (*emit-def* (first defs)))
    (unless (every (lambda (def) (eq (ir-def-result def) result)) defs)
      (emit-error "tail calls between functions with different results"))
    (let ((*result-c-type* (c-type result)))
      `((:struct-def ,struct
                     (:union "u" (:field (:name "char") "none")
                             ,@(loop :for (nil params) :in members
                                     :for i :from 0
                                     :when params
                                       :collect `(:anon-struct ,(format nil "m~d" i) nil
                                                               ,@(loop :for (type) :in params
                                                                       :for j :from 0
                                                                       :collect `(:field ,type ,(format nil "p~d" j)))))))
        (:blank)
        (:comment ,(format nil "~{~a~^, ~}" (mapcar (lambda (def) (symbol-name (ir-def-name def))) defs)))
        (:fn ,*result-c-type* ,function (((:name "int") "entry") ((:struct ,struct) "args"))
             (,@(loop :for (nil params) :in members
                      :append (loop :for (type name) :in params
                                    :collect `(:decl ,type ,name (:init (:int 0)) :unused t)))
              ,@(let ((chain '()))
                  (loop :for (nil params label) :in (reverse members)
                        :for i :downfrom (1- (length members))
                        :do (let ((enter `(,@(loop :for (nil name) :in params
                                                   :for j :from 0
                                                   :collect `(:assign ,(var name) ,(member-field i j)))
                                           (:goto ,label))))
                              (setf chain (if chain
                                              `((:if (:== ,(var "entry") (:int ,i)) ,enter ,chain))
                                              enter))))
                  chain)
              ,@(loop :for (def params label) :in members
                      :append (let* ((*tail-used* nil)
                                     (*tail-calls* (gethash (ir-def-name def) *def-tail-calls*))
                                     (*jumps* jumps))
                                `((:label ,label)
                                  (:block ,@(emit-body def params)))))))
        (:blank)
        ,@(loop :for (def nil nil) :in members
                :for i :from 0
                :append (let* ((*emit-def* def)
                               (params (def-params def)))
                          `((:comment ,(symbol-name (ir-def-name def)))
                            (:fn ,(c-type (ir-def-result def)) ,(c-name (ir-def-name def)) ,params
                                 ((:decl (:struct ,struct) "args" (:init (:int 0)))
                                  ,@(loop :for (nil name) :in params
                                          :for j :from 0
                                          :collect `(:assign ,(member-field i j) ,(var name)))
                                  (:return (:call ,function (:int ,i) ,(var "args")))))
                            (:blank))))))))

(defun emit-main (lowered)
  "The C node of `main`, which prints the root's result."
  (let* ((root (gethash (lowered-root lowered) (lowered-defs lowered)))
         (*emit-def* root)
         (call `(:call ,(c-name (lowered-root lowered)))))
    (when (ir-def-params root)
      (emit-error "a root that takes arguments"))
    (flet ((print-result (format type)
             `(:call "printf" (:string ,format) (:cast (:name ,type) ,call))))
      `(:fn (:name "int") "main" ()
            ((:expr (:call "sheaf_reserve" (:name "sheaf_stacks") (:int ,(lowered-stack-count lowered))))
             (:expr ,(case (numeric-kind (ir-type-name (ir-def-result root)))
                       (:wrap (print-result (format nil "%llu~%") "unsigned long long"))
                       (:trap (print-result (format nil "%lld~%") "long long"))
                       (:float (print-result (format nil "%.17g~%") "double"))
                       (t `(:cast (:name "void") ,call))))
             (:return (:int 0)))
            :public t))))

(defun c-program (assigned)
  "ASSIGNED, a program with its data stacks assigned, as a list of C nodes."
  (check-assigned assigned)
  (let ((*lowered* assigned)
        (*c-types* (make-hash-table :test 'eq))
        (*c-arrays* (make-hash-table :test 'equal))
        (*c-names* (make-hash-table :test 'eq))
        (*c-counter* 0)
        (*emit-def* nil)
        (*def-tail-calls* (make-hash-table :test 'eq))
        (*def-cycle* (make-hash-table :test 'eq))
        (defs (lowered-defs assigned))
        (stacks (max 1 (lowered-stack-count assigned))))
    (maphash (lambda (name def)
               (declare (ignore def))
               (setf (gethash name *c-names*) (fresh-c-name name)))
             defs)
    (maphash (lambda (type definition)
               (declare (ignore definition))
               (struct-name type))
             (lowered-types assigned))
    (let* ((prototypes (loop :for name :being :the :hash-keys :of defs :using (:hash-value def)
                             :collect (let ((*emit-def* def))
                                        `(:proto ,(c-type (ir-def-result def)) ,(c-name name)
                                                 ,@(loop :for (nil . type) :in (ir-def-params def)
                                                         :collect (c-type type))))))
           (functions (progn
                        (find-tail-cycles defs)
                        (loop :for name :being :the :hash-keys :of defs :using (:hash-value def)
                              :for cycle := (gethash name *def-cycle*)
                              :append (cond ((null (rest cycle)) (emit-def def))
                                            ((eq name (first cycle))
                                             (emit-cycle (mapcar (lambda (member) (gethash member defs))
                                                                 cycle)))))))
           (main (emit-main assigned))
           (types (type-definitions assigned)))
      `((:comment "Generated by Sheaf.")
        (:include "sheaf.h")
        (:blank)
        ,@(forward-declarations)
        (:blank)
        ,@(array-definitions '(:slice))
        ,@types
        ,@(array-definitions '(:array :grow :table))
        (:blank)
        (:decl (:name "sheaf_stack") "sheaf_stacks" nil :static t :array ,stacks)
        (:decl (:name "sheaf_bank") "sheaf_banks" nil :static t :array ,stacks :unused t)
        (:blank)
        ,@prototypes
        (:blank)
        ,@functions
        ,main))))

(defun emit-c (assigned &optional stream)
  "ASSIGNED, a program with its data stacks assigned, as a C file that includes
runtime/sheaf.h. Writes it to STREAM, or returns it as a string without one."
  (if stream
      (print-c (c-program assigned) stream)
      (with-output-to-string (stream)
        (emit-c assigned stream))))

(defun write-c (assigned pathname)
  "Write ASSIGNED to the C file PATHNAME. Returns PATHNAME."
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (emit-c assigned stream))
  pathname)
