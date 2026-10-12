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
(defvar *emit-exits* '()
  "The blocks being emitted, innermost first, each its label and, unless its value
is returned from the def, the C variable it assigns, the label after it, the
releases in force at its start and a cell that notes the label is jumped to.")
(defvar *tail-used*)
(defvar *jumps* '() "Where a tail call goes: the callee's name, its label and its parameters, each a C type and name.")
(defvar *tail-calls* '() "The calls in tail position of the def being emitted that jump.")
(defvar *def-tail-calls*)
(defvar *def-cycle*)
(defvar *aux* '() "The functions that expressions need defined before the defs, newest first.")
(defvar *copies* nil "The types whose values a call copies out of the regions of a loop.")

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

(defun copy-needed-p (type &optional seen)
  "Whether copying a value of TYPE follows a cell."
  (and (eq (ir-type-kind type) :data)
       (not (member type seen))
       (or (pooled-p type)
           (some (lambda (field) (copy-needed-p field (cons type seen))) (field-types type)))))

(defun copy-name (type)
  (format nil "copy_~a" (struct-name type)))

(defun copy-call (type value node)
  "A call of the function that copies VALUE, of TYPE, from the regions of the call
NODE to the stack it copies to."
  (setf (gethash type *copies*) t)
  `(:call ,(copy-name type) ,value
          (:addr ,(stack-node (ir-call-region node)))
          (:addr ,(stack-node (ir-call-spare node)))
          (:addr ,(stack-node (ir-call-copy node)))))

(defun call-expr (node env)
  "A C expression for the call NODE, and the statements it needs first."
  (let ((call `(:call ,(c-name (ir-call-function node))
                      ,@(mapcar (lambda (arg) (atom-expr arg env)) (ir-call-args node))))
        (owned (remove nil (list (ir-call-region node) (ir-call-spare node)))))
    (if owned
        (let ((marks (loop :for nil :in owned :collect (fresh-c-name (make-symbol "MARK"))))
              (result (fresh-c-name (make-symbol "RESULT"))))
          (values (var result)
                  (append (loop :for index :in owned
                                :for mark :in marks
                                :collect (mark-declaration :stack index mark))
                          (list `(:decl ,(c-type (ir-node-type node)) ,result ,call))
                          (when (ir-call-copy node)
                            (list `(:assign ,(var result) ,(copy-call (ir-node-type node) (var result) node))))
                          (emit-releases (loop :for index :in owned
                                               :for mark :in marks
                                               :collect (list :stack index mark))))))
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

(defun mark-declaration (kind index mark)
  "The declaration of the C variable MARK, which saves the top of a stack or the
head of a bank."
  (ecase kind
    (:stack `(:decl (:ptr (:name "char")) ,mark (:dot ,(stack-node index) "top") :unused t))
    (:bank `(:decl (:ptr (:name "sheaf_slice")) ,mark (:dot ,(bank-node index) "head") :unused t))))

(defun owned-marks (stacks bank)
  "What a scope that owns STACKS and BANK gives back when it ends, each `:stack`
or `:bank`, its index and the C variable holding its mark, and the declarations
that save the marks."
  (let ((owned (append (loop :for stack :in stacks
                             :collect (list :stack stack (fresh-c-name (make-symbol "MARK"))))
                       (and bank (list (list :bank bank (fresh-c-name (make-symbol "MARK"))))))))
    (values owned
            (loop :for (kind index mark) :in owned
                  :collect (mark-declaration kind index mark)))))

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
          ,@(emit-releases (loop :for (kind index) :in (ir-call-reset node)
                                 :collect (if (eq kind :spare)
                                              (list :stack index (frame-mark index))
                                              (list kind index (iteration-mark kind index)))))
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

;;; Exceptions

(defun type-id (type)
  "The number that identifies the data type TYPE at run time."
  (struct-name type)
  (gethash type *c-types*))

(defun thrown (type)
  "The C expression for the thrown value, which is of the exception type TYPE."
  `(:dot (:dot ,(var "sheaf_thrown") "u") ,(format nil "e~d" (type-id type))))

(defun emit-throw (node env)
  (let ((type (ir-node-type (ir-throw-value node))))
    `((:assign ,(thrown type) ,(atom-expr (ir-throw-value node) env))
      (:expr (:call "sheaf_throw" (:int ,(type-id type)) (:string ,(type-text type)))))))

(defun save-marks (marks bank-marks)
  "The declarations that save the top of each stack in MARKS and the head of each
bank in BANK-MARKS, and the statements that restore them."
  (let ((saves '()) (restores '()))
    (dolist (index marks)
      (let ((mark (fresh-c-name (make-symbol "MARK"))))
        (push `(:decl (:ptr (:name "char")) ,mark (:dot ,(stack-node index) "top")) saves)
        (push `(:assign (:dot ,(stack-node index) "top") ,(var mark)) restores)))
    (dolist (index bank-marks)
      (let ((mark (fresh-c-name (make-symbol "MARK"))))
        (push `(:decl (:ptr (:name "sheaf_slice")) ,mark (:dot ,(bank-node index) "head")) saves)
        (push `(:expr (:call "sheaf_bank_release" (:addr ,(bank-node index)) ,(var mark))) restores)))
    (values (nreverse saves) (nreverse restores))))

(defun open-frame (frame match)
  "The statements that declare the frame FRAME and make it the innermost one."
  `((:decl (:name "sheaf_frame") ,frame nil)
    (:assign (:dot ,(var frame) "prev") ,(var "sheaf_frames"))
    (:assign (:dot ,(var frame) "match") ,(or match '(:name "NULL")))
    (:assign ,(var "sheaf_frames") (:addr ,(var frame)))))

(defun close-frame (frame)
  `(:assign ,(var "sheaf_frames") (:dot ,(var frame) "prev")))

(defun guarded (frame body landing)
  "An `if` that runs BODY with FRAME innermost, and runs LANDING if control lands in the frame."
  `(:if (:== (:call "_setjmp" (:dot ,(var frame) "env")) (:int 0)) ,body ,landing))

(defun catch-match-function (node name)
  "The C function that picks the branch of the catch NODE that the thrown value matches."
  `(:fn (:name "int") ,name (((:name "int") "type"))
        (,@(loop :for branch :in (ir-catch-branches node)
                 :for i :from 0
                 :for type := (ir-catch-branch-type branch)
                 :if type
                   :collect (let ((tests (pattern-code (ir-catch-branch-pattern branch) (thrown type))))
                              `(:if ,(reduce (lambda (a b) `(:&& ,a ,b)) tests
                                             :initial-value `(:== ,(var "type") (:int ,(type-id type))))
                                    ((:return (:int ,i)))))
                 :else
                   :collect `(:return (:int ,i))
                   :and :do (loop-finish))
         ,@(unless (find nil (ir-catch-branches node) :key #'ir-catch-branch-type)
             '((:return (:int -1)))))))

(defun emit-catch-branch (branch target env)
  "The statements of BRANCH, which run with the thrown value bound by its pattern."
  (let ((type (ir-catch-branch-type branch)))
    (if (null type)
        (emit-node (ir-catch-branch-body branch) target env)
        (let ((exception (fresh-c-name (make-symbol "EXCEPTION")))
              (declarations '()))
          (multiple-value-bind (tests bindings) (pattern-code (ir-catch-branch-pattern branch) (var exception))
            (declare (ignore tests))
            (loop :for (name binding-type expression) :in bindings
                  :do (let ((c-name (fresh-c-name name)))
                        (push `(:decl ,(c-type binding-type) ,c-name ,expression :unused t) declarations)
                        (push (cons name c-name) env))))
          (append `((:decl ,(c-type type) ,exception ,(thrown type) :unused t))
                  (nreverse declarations)
                  (emit-node (ir-catch-branch-body branch) target env))))))

(defun emit-catch (node target env)
  "The statements that compute the catch NODE into TARGET: its body with a frame
pushed, then the branch the thrown value matches if control lands in the frame."
  (let* ((frame (fresh-c-name (make-symbol "FRAME")))
         (match (fresh-c-name (make-symbol "MATCH")))
         (result (if (eq target :return) (fresh-c-name (make-symbol "RESULT")) target))
         (branches (ir-catch-branches node))
         (dispatch (list '(:expr (:call "sheaf_no_match")))))
    (push (catch-match-function node match) *aux*)
    (loop :for branch :in (reverse branches)
          :for i :downfrom (1- (length branches))
          :do (setf dispatch `((:if (:== ,(var "sheaf_branch") (:int ,i))
                                    ,(emit-catch-branch branch target env)
                                    ,dispatch))))
    (multiple-value-bind (saves restores) (save-marks (ir-catch-marks node) (ir-catch-bank-marks node))
      `((:block
          ,@saves
          ,@(when (eq target :return) `((:decl ,(c-type (ir-node-type node)) ,result nil :unused t)))
          ,@(open-frame frame (var match))
          ,(guarded frame
                    `(,@(emit-node (ir-catch-body node) result env)
                      ,(close-frame frame)
                      ,@(when (eq target :return) (emit-return (var result))))
                    `(,(close-frame frame)
                      ,@restores
                      ,@dispatch)))))))

(defun emit-protect (node target env)
  "The statements that compute the protect NODE into TARGET: its body with a
frame pushed, and its cleanup after the body returns and after control lands
in the frame, which then goes on unwinding."
  (let* ((frame (fresh-c-name (make-symbol "FRAME")))
         (result (if (eq target :return) (fresh-c-name (make-symbol "RESULT")) target))
         (saved (fresh-c-name (make-symbol "SAVED")))
         (saved-dest (fresh-c-name (make-symbol "DEST")))
         (saved-branch (fresh-c-name (make-symbol "BRANCH"))))
    (flet ((cleanup ()
             (multiple-value-bind (owned saves) (owned-marks (ir-protect-stacks node) (ir-protect-bank node))
               (let ((discarded (fresh-c-name (make-symbol "DISCARDED"))))
                 `((:block
                     ,@saves
                     (:decl ,(c-type (ir-node-type (ir-protect-cleanup node))) ,discarded nil :unused t)
                     ,@(emit-node (ir-protect-cleanup node) discarded env)
                     ,@(emit-releases owned)))))))
      (multiple-value-bind (saves restores) (save-marks (ir-protect-marks node) (ir-protect-bank-marks node))
        `((:block
            ,@saves
            ,@(when (eq target :return) `((:decl ,(c-type (ir-node-type node)) ,result nil :unused t)))
            ,@(open-frame frame nil)
            ,(guarded frame
                      `(,@(emit-node (ir-protect-body node) result env)
                        ,(close-frame frame)
                        ,@(cleanup)
                        ,@(when (eq target :return) (emit-return (var result))))
                      `(,(close-frame frame)
                        ,@restores
                        (:decl (:struct "sheaf_exception") ,saved ,(var "sheaf_thrown"))
                        (:decl (:ptr (:name "sheaf_frame")) ,saved-dest ,(var "sheaf_dest"))
                        (:decl (:name "int") ,saved-branch ,(var "sheaf_branch"))
                        ,@(cleanup)
                        (:assign ,(var "sheaf_thrown") ,(var saved))
                        (:assign ,(var "sheaf_dest") ,(var saved-dest))
                        (:assign ,(var "sheaf_branch") ,(var saved-branch))
                        (:expr (:call "sheaf_unwind"))))))))))

(defun emit-block (node target env)
  "The statements that compute the block NODE into TARGET: its body, then the
label that a return out of it jumps to."
  (let ((label (ir-block-label node)))
    (if (eq target :return)
        (let ((*emit-exits* (acons label '(:return) *emit-exits*)))
          (emit-node (ir-block-body node) :return env))
        (let* ((end (fresh-c-name (make-symbol "END")))
               (jumped (list nil))
               (body (let ((*emit-exits* (acons label (list target end *releases* jumped) *emit-exits*)))
                       (emit-node (ir-block-body node) target env))))
          (append body (when (first jumped) `((:label ,end))))))))

(defun emit-block-return (node env)
  "The statements of the return NODE: give back what was taken since its block began, then leave it."
  (let ((value (atom-expr (ir-return-value node) env))
        (exit (cdr (assoc (ir-return-label node) *emit-exits*))))
    (if (equal exit '(:return))
        (emit-return value)
        (destructuring-bind (target end releases jumped) exit
          (setf (first jumped) t)
          `((:assign ,(var target) ,value)
            ,@(emit-releases (ldiff *releases* releases))
            (:goto ,end))))))

(defun emit-loop-steps (node vars env)
  "The statements that give each loop variable of NODE with a step its next
value: all at once, or one after another in a `for*`."
  (let ((steps (loop :for (nil c-name type) :in vars
                     :for step :in (ir-loop-steps node)
                     :when step :collect (list c-name type step))))
    (if (ir-loop-sequential node)
        (loop :for (c-name nil step) :in steps
              :collect `(:block ,@(emit-node step c-name env)))
        (let ((temps (loop :for (c-name type step) :in steps
                           :collect (list c-name (fresh-c-name (make-symbol "NEXT")) type step))))
          `((:block
              ,@(loop :for (nil temp type step) :in temps
                      :append `((:decl ,(c-type type) ,temp nil :unused t)
                                ,@(emit-node step temp env)))
              ,@(loop :for (c-name temp) :in temps
                      :collect `(:assign ,(var c-name) ,(var temp)))))))))

(defun emit-loop (node target env)
  "The statements that compute the loop NODE into TARGET: the variables, then its
test, body and steps again and again until the test or a break leaves, then its result."
  (let* ((top (fresh-c-name (make-symbol "LOOP")))
         (done (fresh-c-name (make-symbol "DONE")))
         (step (fresh-c-name (make-symbol "STEP")))
         (done-used (list nil))
         (step-used (list nil))
         (vars (loop :for (name . type) :in (ir-loop-vars node)
                     :collect (list name (fresh-c-name name) type)))
         (inner (append (loop :for (name c-name) :in vars :collect (cons name c-name)) env))
         (test (ir-loop-test node))
         (*emit-exits* (acons (ir-loop-label node)
                              (list :loop done step *releases* done-used step-used)
                              *emit-exits*))
         (test-statements
           (when test
             (let* ((result (fresh-c-name (make-symbol "TEST")))
                    (type (ir-node-type test))
                    (exit (ir-constructor-tag (named-constructor *lowered* type
                                                                 (if (eq (ir-loop-exit-on node) :true) "TRUE" "FALSE")))))
               (setf (first done-used) t)
               `((:block
                   (:decl ,(c-type type) ,result nil :unused t)
                   ,@(emit-node test result inner)
                   (:if (:== (:dot ,(var result) "tag") (:int ,exit)) ((:goto ,done))))))))
         (discarded (fresh-c-name (make-symbol "DISCARDED")))
         (body `((:block
                   (:decl ,(c-type (ir-node-type (ir-loop-body node))) ,discarded nil :unused t)
                   ,@(emit-node (ir-loop-body node) discarded inner))))
         (steps (emit-loop-steps node vars inner))
         (resets (loop :for index :in (ir-loop-reset node)
                       :collect (list :stack index (fresh-c-name (make-symbol "ITER"))))))
    `((:block
        ,@(loop :for (nil c-name type) :in vars
                :for init :in (ir-loop-inits node)
                :collect `(:decl ,(c-type type) ,c-name ,(atom-expr init env) :unused t))
        ,@(loop :for (kind index mark) :in resets
                :collect (mark-declaration kind index mark))
        (:label ,top)
        ,@test-statements
        ,@body
        ,@(when (first step-used) `((:label ,step)))
        ,@steps
        ,@(emit-releases resets)
        (:goto ,top)
        ,@(when (first done-used) `((:label ,done)))
        ,@(emit-node (ir-loop-returns node) target inner)))))

(defun emit-loop-exit (node)
  "The statements of the break or continue NODE: give back what was taken since
its loop's iteration began, then jump."
  (let ((breakp (ir-break-p node)))
    (destructuring-bind (done step releases done-used step-used)
        (cddr (assoc (if breakp (ir-break-label node) (ir-continue-label node)) *emit-exits*))
      (setf (first (if breakp done-used step-used)) t)
      `(,@(emit-releases (ldiff *releases* releases))
        (:goto ,(if breakp done step))))))

(defun emit-node (node target env)
  "The statements that compute NODE into TARGET, a C variable or `:return`."
  (etypecase node
    (ir-let
     (let ((name (fresh-c-name (ir-let-name node))))
       (multiple-value-bind (owned saves) (owned-marks (ir-let-stacks node) (ir-let-bank node))
         (let ((*releases* (append owned *releases*)))
           (append
            saves
            `((:decl ,(c-type (ir-node-type (ir-let-value node))) ,name nil :unused t))
            (emit-node (ir-let-value node) name env)
            (emit-node (ir-let-body node) target (acons (ir-let-name node) name env))
            (unless (eq target :return)
              (emit-releases owned)))))))
    (ir-block (emit-block node target env))
    (ir-loop (emit-loop node target env))
    ((or ir-break ir-continue) (emit-loop-exit node))
    (ir-return (emit-block-return node env))
    (ir-match (emit-match node target env))
    (ir-throw (emit-throw node env))
    (ir-catch (emit-catch node target env))
    (ir-protect (emit-protect node target env))
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

(defun iteration-mark (kind index)
  "The name of the C variable that saves a stack or bank at each iteration's start."
  (format nil "iter_mark_~(~a~)_~d" kind index))

(defun frame-mark (index)
  "The name of the C variable that saves a spare region when a cycle is entered."
  (format nil "frame_mark_~d" index))

(defun tail-resets ()
  "The stacks and banks the tail calls being emitted reset to where the iteration
began, each its kind and index."
  (remove-duplicates (loop :for call :in *tail-calls*
                           :append (remove :spare (ir-call-reset call) :key #'first))
                     :test #'equal))

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
       `((:decl (:ptr (:name "char")) "pool_mark" (:dot ,(stack-node (ir-def-pool def)) "top") :unused t)))
     (loop :for (kind index) :in (tail-resets)
           :collect (mark-declaration kind index (iteration-mark kind index)))
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

(defun spare-regions (defs)
  "The spare regions the tail calls of the cycle DEFS switch to, which the cycle
gives back to what they held when it was entered."
  (remove-duplicates
   (loop :for def :in defs
         :append (loop :for call :in (gethash (ir-def-name def) *def-tail-calls*)
                       :append (loop :for (kind index) :in (ir-call-reset call)
                                     :when (eq kind :spare) :collect index)))))

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
              ,@(loop :for index :in (spare-regions defs)
                      :collect (mark-declaration :stack index (frame-mark index)))
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

(defun scan-program (defs)
  "The exception types DEFS throw or catch, and whether any def throws, catches
or protects."
  (let ((types '()) (handlers nil))
    (maphash (lambda (name def)
               (declare (ignore name))
               (walk-ir (lambda (node)
                          (typecase node
                            (ir-throw (setf handlers t)
                             (pushnew (ir-node-type (ir-throw-value node)) types))
                            (ir-catch (setf handlers t)
                             (dolist (branch (ir-catch-branches node))
                               (when (ir-catch-branch-type branch)
                                 (pushnew (ir-catch-branch-type branch) types))))
                            (ir-protect (setf handlers t))))
                        (ir-def-body def)))
             defs)
    (values (nreverse types) handlers)))

(defun copy-definition (type)
  "The C function that copies a value of TYPE, from the regions A and B to the
stack TO: a cell on either is copied, and any other is shared."
  ;; TODO: the copy visits a shared cell once for each path to it, and calls itself once for each cell on a path
  ;; https://github.com/takeiteasy/sheaf/issues/34
  (let* ((pooled (pooled-p type))
         (struct `(:struct ,(struct-name type)))
         (source (if pooled "p" "value"))
         (target (if pooled "cell" "out"))
         (stack '(:ptr (:name "sheaf_stack")))
         (copies (loop :for constructor :in (lowered-constructors *lowered* type)
                       :for tag := (ir-constructor-tag constructor)
                       :for fields := (loop :for field :in (ir-constructor-fields constructor)
                                            :for i :from 0
                                            :when (copy-needed-p field)
                                              :collect (let ((member (list "u" (format nil "c~d" tag) (format nil "f~d" i))))
                                                         `(:assign ,(apply #'member-access (var target) type member)
                                                                   (:call ,(copy-name field)
                                                                          ,(apply #'member-access (var source) type member)
                                                                          ,(var "a") ,(var "b") ,(var "to")))))
                       :when fields
                         :collect `(:if (:== ,(member-access (var source) type "tag") (:int ,tag)) ,fields))))
    `(:fn ,(c-type type) ,(copy-name type)
          ((,(c-type type) ,source) (,stack "a") (,stack "b") (,stack "to"))
          ,(if pooled
               `((:if (:== (:call "sheaf_in_regions" ,(var "a") ,(var "b") ,(var source)) (:int 0))
                      ((:return ,(var source))))
                 (:decl (:ptr ,struct) "cell" (:call "sheaf_push" ,(var "to") (:sizeof ,struct) (:alignof ,struct)))
                 (:assign (:deref ,(var "cell")) (:deref ,(var source)))
                 ,@copies
                 (:return ,(var "cell")))
               `((:decl ,(c-type type) "out" ,(var source))
                 ,@copies
                 (:return ,(var "out")))))))

(defun copy-definitions ()
  "The prototypes and definitions of the functions that copy the types in `*copies*`
and the types they hold."
  (let ((types '())
        (pending (loop :for type :being :the :hash-keys :of *copies* :collect type)))
    (loop :while pending
          :do (let ((type (pop pending)))
                (unless (member type types)
                  (push type types)
                  (dolist (field (field-types type))
                    (when (copy-needed-p field)
                      (push field pending))))))
    (setf types (sort types #'< :key (lambda (type) (gethash type *c-types*))))
    (let ((stack '(:ptr (:name "sheaf_stack"))))
      (append (loop :for type :in types
                    :collect `(:proto ,(c-type type) ,(copy-name type) ,(c-type type) ,stack ,stack ,stack))
              (loop :for type :in types
                    :append (list (copy-definition type) '(:blank)))))))

(defun exception-definitions (types)
  "The C struct that holds the value being thrown, a union of the exception TYPES."
  `((:struct-def "sheaf_exception"
                 (:union "u" (:field (:name "char") "none")
                         ,@(loop :for type :in types
                                 :collect `(:field ,(c-type type) ,(format nil "e~d" (type-id type))))))
    (:decl (:struct "sheaf_exception") "sheaf_thrown" nil :static t :unused t)
    (:blank)))

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
        (*aux* '())
        (*copies* (make-hash-table :test 'eq))
        (defs (lowered-defs assigned))
        (stacks (max 1 (lowered-stack-count assigned)))
        (exception-types '())
        (handlers nil))
    (multiple-value-setq (exception-types handlers) (scan-program defs))
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
           (types (type-definitions assigned))
           (copies (copy-definitions)))
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
        ,@(when handlers (exception-definitions exception-types))
        ,@copies
        ,@prototypes
        (:blank)
        ,@(loop :for function :in (reverse *aux*) :append `(,function (:blank)))
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
