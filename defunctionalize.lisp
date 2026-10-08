;;;; defunctionalize.lisp
;;;; Turning function values into tagged unions of the lambdas that reach them.

(in-package #:sheaf)

;;; Shapes
;;;
;;; A shape mirrors a ground type and marks where function values and the
;;; data holding them sit. Every function type has a lambda set (a class),
;;; and sets unify with the types they flow through.

(defstruct (cls (:constructor make-cls ()))
  "A lambda set: the lambdas that can reach a function type."
  (parent nil)
  (cases '()))

(defstruct (lcase (:constructor make-lcase (lam captures)))
  "A lambda in a set, with the shapes of the variables it captures."
  lam
  captures)

(defstruct (lam (:constructor make-lam (id owner node target)))
  "A lambda site, or a global used as a value when TARGET names it."
  id
  owner
  node
  target
  (free '())
  (free-shapes '())
  (params '())
  (ret nil)
  (group '())
  (self nil))

(defstruct (shape (:constructor nil))
  (ty nil))

(defstruct (atom-shape (:include shape) (:constructor make-atom-shape (ty)))
  "A type with no function values in it.")

(defstruct (fn-shape (:include shape))
  cls args ret)

(defstruct (data-shape (:include shape))
  "A data type holding function values. CTORS pairs each constructor with the
shapes of its fields. Unified shapes share a root through PARENT."
  args ctors (parent nil))

(defun find-data (shape)
  (loop :while (data-shape-parent shape)
        :do (setf shape (data-shape-parent shape)))
  shape)

(defun find-cls (cls)
  (loop :while (cls-parent cls)
        :do (let ((parent (cls-parent cls)))
              (when (cls-parent parent)
                (setf (cls-parent cls) (cls-parent parent)))
              (setf cls parent)))
  cls)

(defvar *unified*)

(defun add-case (cls lcase)
  (let ((existing (find (lcase-lam lcase) (cls-cases cls) :key #'lcase-lam)))
    (if existing
        (mapc #'unify-shapes (lcase-captures existing) (lcase-captures lcase))
        (push lcase (cls-cases cls)))))

(defun merge-classes (a b)
  (let ((a (find-cls a))
        (b (find-cls b)))
    (unless (eq a b)
      (setf (cls-parent a) b)
      (let ((cases (cls-cases a)))
        (setf (cls-cases a) '())
        (dolist (lcase cases)
          (add-case b lcase))))))

(defun unify-shapes (a b)
  (unless (or (eq a b) (member b (gethash a *unified*)))
    (push b (gethash a *unified*))
    (push a (gethash b *unified*))
    (etypecase a
      (atom-shape)
      (fn-shape
       (unless (fn-shape-p b)
         (error "Cannot unify ~a with ~a" (tc:type-to-string (shape-ty a)) (tc:type-to-string (shape-ty b))))
       (merge-classes (fn-shape-cls a) (fn-shape-cls b))
       (mapc #'unify-shapes (fn-shape-args a) (fn-shape-args b))
       (unify-shapes (fn-shape-ret a) (fn-shape-ret b)))
      (data-shape
       (unless (data-shape-p b)
         (error "Cannot unify ~a with ~a" (tc:type-to-string (shape-ty a)) (tc:type-to-string (shape-ty b))))
       (let ((root-a (find-data a))
             (root-b (find-data b)))
         (unless (eq root-a root-b)
           (setf (data-shape-parent root-a) root-b))
         (mapc #'unify-shapes (data-shape-args a) (data-shape-args b))
         (loop :for (nil . fields-a) :in (data-shape-ctors a)
               :for (nil . fields-b) :in (data-shape-ctors b)
               :do (mapc #'unify-shapes fields-a fields-b)))))))

(defun shape-has-fn-p (shape)
  (not (atom-shape-p shape)))

(defun copy-shape (shape memo)
  "A copy of SHAPE with fresh lambda sets; MEMO keeps shared parts shared."
  (or (gethash shape memo)
      (etypecase shape
        (atom-shape shape)
        (fn-shape
         (let ((new (make-fn-shape :ty (shape-ty shape))))
           (setf (gethash shape memo) new
                 (fn-shape-cls new) (copy-cls (fn-shape-cls shape) memo)
                 (fn-shape-args new) (copy-shapes (fn-shape-args shape) memo)
                 (fn-shape-ret new) (copy-shape (fn-shape-ret shape) memo))
           new))
        (data-shape
         (let* ((root (find-data shape))
                (new (or (gethash root memo)
                         (let ((new (make-data-shape :ty (shape-ty root))))
                           (setf (gethash root memo) new
                                 (data-shape-args new) (copy-shapes (data-shape-args root) memo)
                                 (data-shape-ctors new) (loop :for (ctor . fields) :in (data-shape-ctors root)
                                                              :collect (cons ctor (copy-shapes fields memo))))
                           new))))
           (setf (gethash shape memo) new))))))

(defun copy-shapes (shapes memo)
  (mapcar (lambda (shape) (copy-shape shape memo)) shapes))

(defun copy-cls (cls memo)
  (let ((root (find-cls cls)))
    (or (gethash root memo)
        (let ((new (make-cls)))
          (setf (gethash root memo) new
                (cls-cases new) (loop :for lcase :in (cls-cases root)
                                      :collect (make-lcase (lcase-lam lcase)
                                                           (copy-shapes (lcase-captures lcase) memo))))
          new))))

;;; Shapes of types

(defvar *owns-function*)

(defun type-mentions-function-p (type)
  (etypecase type
    (tc:function-ty t)
    (tc:tapp (or (type-mentions-function-p (tc:tapp-from type))
                 (type-mentions-function-p (tc:tapp-to type))))
    (tc:tycon (tycon-owns-function-p (tc:tycon-name type)))
    (tc:tyvar nil)
    (tc:result-ty (some #'type-mentions-function-p (tc:result-ty-output-types type)))))

(defun constructor-field-types (constructor)
  (tc:constructor-arguments constructor entry:*global-environment*))

(defun tycon-owns-function-p (name)
  "Whether a constructor of the type NAME has a field that is, or contains, a
function type. Fields that are only type parameters do not count."
  (case (gethash name *owns-function*)
    (:yes t)
    ((:no :pending) nil)
    (t
     (setf (gethash name *owns-function*) :pending)
     (let* ((entry (tc:lookup-type entry:*global-environment* name :no-error t))
            (owns (and entry
                       (some (lambda (constructor)
                               (some #'type-mentions-function-p (constructor-field-types constructor)))
                             (tc:type-entry-constructors entry)))))
       (setf (gethash name *owns-function*) (if owns :yes :no))
       owns))))

(defun substitute-types (type tyvars)
  "TYPE with each type variable of TYVARS, an alist of variable and shape, replaced
by its shape's type."
  (if tyvars
      (tc:apply-substitution (mapcar (lambda (entry)
                                       (tc:make-substitution :from (car entry) :to (shape-ty (cdr entry))))
                                     tyvars)
                             type)
      type))

(defun unsupported (control &rest args)
  (error 'specialization-error
         :message (format nil "Defunctionalization does not support ~?" control args)
         :chain (current-chain)))

(defvar *owner* nil
  "The specialization being analyzed and the node being looked at, for errors.")
(defvar *at* nil)

(defun current-chain ()
  (and *owner*
       (list (list (car *owner*)
                   (if *at* (ast:node-type *at*) (cdr *owner*))
                   (and *at* (ast:node-location *at*))))))

(defun shape-of (type &optional tyvars path)
  "A fresh shape for TYPE. TYVARS maps the type variables TYPE may mention to
shapes, and PATH holds the data shapes being built."
  (etypecase type
    (tc:function-ty
     (when (or (tc:function-ty-keyword-input-types type) (tc:function-ty-keyword-open-p type))
       (unsupported "keyword arguments"))
     (make-fn-shape
      :ty (substitute-types type tyvars)
      :cls (make-cls)
      :args (mapcar (lambda (input) (shape-of input tyvars path))
                    (tc:function-ty-positional-input-types type))
      :ret (shape-of (tc:output-types-result-type (tc:function-ty-output-types type)) tyvars path)))
    (tc:tyvar
     (or (cdr (assoc (tc:tyvar-id type) tyvars :key #'tc:tyvar-id))
         (make-atom-shape type)))
    ((or tc:tycon tc:tapp)
     (data-shape-of type tyvars path))
    (tc:result-ty
     (when (type-mentions-function-p type)
       (unsupported "multiple values holding functions"))
     (make-atom-shape type))))

(defconstant +shape-depth-limit+ 16)

(defun data-shape-of (type tyvars path)
  (let* ((flat (tc:flatten-type type))
         (head (first flat)))
    (if (not (tc:tycon-p head))
        (make-atom-shape (substitute-types type tyvars))
        (let ((args (mapcar (lambda (arg) (shape-of arg tyvars path)) (rest flat))))
          (if (not (or (tycon-owns-function-p (tc:tycon-name head)) (some #'shape-has-fn-p args)))
              (make-atom-shape (substitute-types type tyvars))
              (or (find-if (lambda (ancestor)
                             (and (eq (tc:tycon-name head) (tc:tycon-name (first (tc:flatten-type (shape-ty ancestor)))))
                                  (every #'eq args (data-shape-args ancestor))))
                           path)
                  (progn
                    (when (> (length path) +shape-depth-limit+)
                      (unsupported "the unbounded data type ~a" (tc:type-to-string type)))
                    (let ((shape (make-data-shape :ty (substitute-types type tyvars) :args args))
                          (entry (tc:lookup-type entry:*global-environment* (tc:tycon-name head) :no-error t)))
                      (setf (data-shape-ctors shape)
                            (loop :for constructor :in (and entry (tc:type-entry-constructors entry))
                                  :collect (cons constructor
                                                 (constructor-field-shapes constructor args (cons shape path)))))
                      shape))))))))

(defun constructor-field-shapes (constructor args path)
  "Shapes for the fields of CONSTRUCTOR of a type applied to the shapes ARGS."
  (let* ((scheme (tc:lookup-value-type entry:*global-environment* constructor))
         (type (tc:qualified-ty-type (tc:fresh-inst scheme)))
         (result (if (tc:function-type-p type) (tc:function-return-type type) type))
         (tyvars (loop :for var :in (rest (tc:flatten-type result))
                       :for arg :in args
                       :collect (cons var arg))))
    (mapcar (lambda (field) (shape-of field tyvars path))
            (tc:function-type-arguments type))))

;;; Analysis

(defstruct (binfo (:constructor make-binfo (name node kind params ret)))
  "What the analysis knows of one specialized binding. A function has PARAMS
and RET shapes; a value has just RET."
  name node kind params ret (state :open))

(defvar *shapes*)
(defvar *sites*)
(defvar *binfos*)
(defvar *lams*)
(defvar *global-lams*)
(defvar *group-lams*)

(defun node-children (node)
  (typecase node
    ((or ast:node-literal ast:node-variable ast:node-lisp) '())
    (ast:node-application
     (when (ast:node-application-keyword-rands node)
       (unsupported "keyword arguments"))
     (cons (ast:node-application-rator node) (ast:node-application-rands node)))
    (ast:node-direct-application
     (when (ast:node-direct-application-keyword-rands node)
       (unsupported "keyword arguments"))
     (ast:node-direct-application-rands node))
    (ast:node-abstraction (list (ast:node-abstraction-subexpr node)))
    (ast:node-let (append (mapcar #'cdr (ast:node-let-bindings node))
                          (list (ast:node-let-subexpr node))))
    (ast:node-bind (list (ast:node-bind-expr node) (ast:node-bind-body node)))
    (ast:node-seq (ast:node-seq-nodes node))
    (ast:node-locally (list (ast:node-locally-subexpr node)))
    (ast:node-match (cons (ast:node-match-expr node)
                          (mapcar #'match-branch-body-of (ast:node-match-branches node))))
    ;; TODO: only the node kinds the fixtures use; the rest signal an error.
    ;; https://todo.sr.ht/~takeiteasy/sheaf/20
    (t (unsupported "~(~a~) nodes" (type-of node)))))

(defun match-branch-body-of (branch)
  (ast:match-branch-body branch))

(defun referenced-names (node)
  "Every local or global name NODE refers to, in the order first seen."
  (let ((names '()))
    (labels ((walk (node)
               (typecase node
                 (ast:node-variable (pushnew (ast:node-variable-value node) names))
                 (ast:node-direct-application (pushnew (ast:node-direct-application-rator node) names))
                 (ast:node-lisp (dolist (var (ast:node-lisp-vars node))
                                  (pushnew (cdr var) names))))
               (mapc #'walk (node-children node))))
      (walk node))
    (nreverse names)))

(defun free-locals (node env)
  "The names NODE refers to that ENV binds."
  (remove-if-not (lambda (name) (assoc name env)) (referenced-names node)))

;;; Bindings and their call graph

(defun binding-sccs (bindings)
  "The names of BINDINGS, a table of name to node, in strongly connected
components, each after the ones it refers to."
  (let ((index 0) (stack '()) (info (make-hash-table :test 'eq)) (sccs '()))
    (labels ((edges (name)
               (remove-if-not (lambda (ref) (nth-value 1 (gethash ref bindings)))
                              (referenced-names (gethash name bindings))))
             (visit (name)
               (let ((entry (list index index t)))
                 (setf (gethash name info) entry)
                 (incf index)
                 (push name stack)
                 (dolist (next (edges name))
                   (let ((other (gethash next info)))
                     (cond ((null other)
                            (visit next)
                            (setf (first entry) (min (first entry) (first (gethash next info)))))
                           ((third other)
                            (setf (first entry) (min (first entry) (second other)))))))
                 (when (= (first entry) (second entry))
                   (let ((scc '()))
                     (loop :for member := (pop stack)
                           :do (push member scc)
                               (setf (third (gethash member info)) nil)
                           :until (eq member name))
                     (push scc sccs))))))
      (maphash (lambda (name node)
                 (declare (ignore node))
                 (unless (gethash name info) (visit name)))
               bindings))
    (nreverse sccs)))

;;; Inference

(defun new-lam (node target)
  (let ((lam (make-lam (length *lams*) *owner* node target)))
    (push lam *lams*)
    lam))

(defun global-lam (name)
  (or (gethash name *global-lams*)
      (setf (gethash name *global-lams*) (new-lam nil name))))

(defun instantiate-binding (binfo)
  "The shapes a use of BINFO sees: a copy once the binding is analyzed, since
its sets are then free to differ per use."
  (let ((shapes (cons (binfo-ret binfo) (binfo-params binfo))))
    (if (eq (binfo-state binfo) :done)
        (let ((copies (copy-shapes shapes (make-hash-table :test 'eq))))
          (values (rest copies) (first copies)))
        (values (rest shapes) (first shapes)))))

(defun infer (node env)
  (let* ((*at* node)
         (shape (infer-node node env)))
    (setf (gethash node *shapes*) shape)
    shape))

(defun call-shape (fn args)
  "The result shape of calling the function shape FN with ARGS."
  (unless (fn-shape-p fn)
    (unsupported "calling a value that is not a function"))
  (unless (= (length args) (length (fn-shape-args fn)))
    (unsupported "partial or over-application"))
  (mapc #'unify-shapes (fn-shape-args fn) args)
  (fn-shape-ret fn))

(defun call-global (name args)
  (multiple-value-bind (params ret) (instantiate-binding (gethash name *binfos*))
    (unless (= (length args) (length params))
      (unsupported "partial or over-application"))
    (mapc #'unify-shapes params args)
    (values ret params)))

(defun global-name-p (name env)
  (and (not (assoc name env)) (gethash name *binfos*)))

(defun infer-node (node env)
  (typecase node
    (ast:node-literal (shape-of (ast:node-type node)))
    (ast:node-variable (infer-variable node env))
    (ast:node-application (infer-application node env))
    (ast:node-direct-application (infer-direct node env))
    (ast:node-abstraction (infer-lambda node env '() '()))
    (ast:node-let (infer-let node env))
    (ast:node-bind
     (let ((value (infer (ast:node-bind-expr node) env)))
       (infer (ast:node-bind-body node) (acons (ast:node-bind-name node) value env))))
    (ast:node-seq (let ((shape nil))
                    (dolist (child (ast:node-seq-nodes node) shape)
                      (setf shape (infer child env)))))
    (ast:node-locally (infer (ast:node-locally-subexpr node) env))
    (ast:node-match (infer-match node env))
    (ast:node-lisp (infer-lisp node env))
    (t (unsupported "~(~a~) nodes" (type-of node)))))

(defun infer-variable (node env)
  (let* ((name (ast:node-variable-value node))
         (local (assoc name env)))
    (cond (local (cdr local))
          ((gethash name *binfos*)
           (let ((binfo (gethash name *binfos*)))
             (ecase (binfo-kind binfo)
               (:value (nth-value 1 (instantiate-binding binfo)))
               (:function
                (multiple-value-bind (params ret) (instantiate-binding binfo)
                  (let ((shape (make-fn-shape :ty (ast:node-type node) :cls (make-cls)
                                              :args params :ret ret)))
                    (add-case (fn-shape-cls shape) (make-lcase (global-lam name) '()))
                    shape))))))
          ((constructor-p name)
           (when (typep (ast:node-type node) (quote tc:function-ty))
             (unsupported "the constructor ~s used as a function" name))
           (infer-constructor node '()))
          (t (let ((shape (shape-of (ast:node-type node))))
               (when (shape-has-fn-p shape)
                 (unsupported "~s, which has no code to analyze, holding a function" name))
               shape)))))

(defun infer-constructor (node args)
  (let* ((shape (shape-of (if args
                              (tc:output-types-result-type
                               (tc:function-ty-output-types (ast:node-type node)))
                              (ast:node-type node))))
         (name (ast:node-variable-value node)))
    (when (and (data-shape-p shape) args)
      (let ((fields (cdr (assoc name (data-shape-ctors shape)))))
        (mapc #'unify-shapes fields args)))
    shape))

(defun infer-direct (node env)
  (let* ((name (ast:node-direct-application-rator node))
         (args (mapcar (lambda (rand) (infer rand env)) (ast:node-direct-application-rands node))))
    (when (ast:node-direct-application-keyword-rands node)
      (unsupported "keyword arguments"))
    (cond ((assoc name env)
           (setf (gethash node *sites*) (cdr (assoc name env)))
           (call-shape (cdr (assoc name env)) args))
          ((gethash name *binfos*)
           (multiple-value-bind (ret params) (call-global name args)
             (setf (gethash node *sites*) (cons params ret))
             ret))
          ((constructor-p name)
           (let ((shape (shape-of (ast:node-type node))))
             (when (data-shape-p shape)
               (mapc #'unify-shapes (cdr (assoc name (data-shape-ctors shape))) args))
             shape))
          (t (infer-opaque-call node args)))))

(defun infer-opaque-call (node args)
  (let ((shape (shape-of (ast:node-type node))))
    (when (or (shape-has-fn-p shape) (some #'shape-has-fn-p args))
      (unsupported "~s, which has no code to analyze, receiving a function"
                   (ast:node-direct-application-rator node)))
    shape))

(defun infer-application (node env)
  (let ((rator (ast:node-application-rator node))
        (rands (ast:node-application-rands node)))
    (when (ast:node-application-keyword-rands node)
      (unsupported "keyword arguments"))
    (let ((global (and (typep rator 'ast:node-variable)
                       (global-name-p (ast:node-variable-value rator) env)
                       (eq (binfo-kind (gethash (ast:node-variable-value rator) *binfos*)) :function)
                       (ast:node-variable-value rator)))
          (constructor (and (typep rator 'ast:node-variable)
                            (not (assoc (ast:node-variable-value rator) env))
                            (not (gethash (ast:node-variable-value rator) *binfos*))
                            (constructor-p (ast:node-variable-value rator)))))
      (cond (global
             (let ((args (mapcar (lambda (rand) (infer rand env)) rands)))
               (multiple-value-bind (ret params) (call-global global args)
                 (setf (gethash node *sites*) (cons params ret))
                 ret)))
            (constructor
             (infer-constructor rator (mapcar (lambda (rand) (infer rand env)) rands)))
            (t
             (let* ((fn (infer rator env))
                    (args (mapcar (lambda (rand) (infer rand env)) rands)))
               (call-shape fn args)))))))

(defun infer-lambda (node env group group-free)
  "The shape of the lambda NODE. GROUP names the recursive bindings it belongs
to; they are rebuilt where used, so are not captured."
  (when (ast:node-abstraction-keyword-params node)
    (unsupported "keyword arguments"))
  (let* ((lam (new-lam node nil))
         (free (or group-free
                   (remove-if (lambda (name) (member name group))
                              (free-locals (ast:node-abstraction-subexpr node) env))))
         (type (ast:node-type node))
         (params (mapcar #'shape-of (tc:function-ty-positional-input-types type)))
         (ret (shape-of (tc:output-types-result-type (tc:function-ty-output-types type))))
         (captured (mapcar (lambda (name) (cdr (assoc name env))) free))
         (body-env (append (mapcar #'cons (ast:node-abstraction-vars node) params) env)))
    (setf (gethash node *sites*) lam
          (lam-free lam) free
          (lam-free-shapes lam) captured
          (lam-params lam) params
          (lam-ret lam) ret
          (lam-group lam) group)
    (unify-shapes ret (infer (ast:node-abstraction-subexpr node) body-env))
    (let ((shape (make-fn-shape :ty type :cls (make-cls) :args params :ret ret)))
      (add-case (fn-shape-cls shape) (make-lcase lam captured))
      shape)))

(defun infer-let (node env)
  (let* ((bindings (ast:node-let-bindings node))
         (shapes (mapcar (lambda (binding) (shape-of (ast:node-type (cdr binding)))) bindings))
         (env (append (mapcar (lambda (binding shape) (cons (car binding) shape)) bindings shapes) env))
         (lambdas (remove-if-not (lambda (binding) (typep (cdr binding) 'ast:node-abstraction)) bindings))
         (group (mapcar #'car lambdas))
         (recursive (some (lambda (binding)
                            (intersection group (referenced-names (cdr binding))))
                          lambdas))
         (group-free (and recursive
                          (remove-duplicates
                           (loop :for binding :in lambdas
                                 :append (remove-if (lambda (name) (member name group))
                                                    (free-locals (cdr binding) env)))
                           :from-end t))))
    (loop :for binding :in bindings
          :for shape :in shapes
          :do (let ((value (if (and recursive (member (car binding) group))
                               (let ((value (infer-lambda (cdr binding) env group group-free)))
                                 (setf (gethash (cdr binding) *shapes*) value)
                                 (let ((lam (gethash (cdr binding) *sites*)))
                                   (setf (lam-self lam) (car binding)
                                         (gethash (car binding) *group-lams*) lam))
                                 value)
                               (infer (cdr binding) env))))
                (unify-shapes shape value)))
    (infer (ast:node-let-subexpr node) env)))

(defun infer-match (node env)
  (let ((scrutinee (infer (ast:node-match-expr node) env))
        (result nil))
    (dolist (branch (ast:node-match-branches node) result)
      (let ((shape (infer (ast:match-branch-body branch)
                          (bind-pattern (ast:match-branch-pattern branch) scrutinee env))))
        (if result
            (unify-shapes result shape)
            (setf result shape))))))

(defun bind-pattern (pattern shape env)
  (setf (gethash pattern *shapes*) shape)
  (etypecase pattern
    (pattern:pattern-var (acons (pattern:pattern-var-name pattern) shape env))
    ((or pattern:pattern-wildcard pattern:pattern-literal) env)
    (pattern:pattern-binding
     (bind-pattern (pattern:pattern-binding-pattern pattern) shape
                   (bind-pattern (pattern:pattern-binding-var pattern) shape env)))
    (pattern:pattern-constructor
     (let ((fields (and (data-shape-p shape)
                        (cdr (assoc (pattern:pattern-constructor-name pattern) (data-shape-ctors shape))))))
       (loop :for sub :in (pattern:pattern-constructor-patterns pattern)
             :for i :from 0
             :do (setf env (bind-pattern sub (or (nth i fields) (shape-of (pattern:pattern-type sub))) env)))
       env))))

(defun infer-lisp (node env)
  (dolist (var (ast:node-lisp-vars node))
    (let ((local (assoc (cdr var) env)))
      (when (and local (shape-has-fn-p (cdr local)))
        (unsupported "raw Lisp receiving the function ~s" (cdr var)))))
  (let ((shape (shape-of (ast:node-type node))))
    (when (shape-has-fn-p shape)
      (unsupported "raw Lisp returning a function"))
    shape))

(defun infer-binding (binfo)
  (let* ((node (binfo-node binfo))
         (*owner* (cons (binfo-name binfo) (ast:node-type node))))
    (ecase (binfo-kind binfo)
      (:function
       (let ((env (mapcar #'cons (ast:node-abstraction-vars node) (binfo-params binfo))))
         (unify-shapes (binfo-ret binfo) (infer (ast:node-abstraction-subexpr node) env))))
      (:value
       (unify-shapes (binfo-ret binfo) (infer node '()))))))

(defun make-binfo-for (name node)
  (let ((*owner* (cons name (ast:node-type node))))
    (if (typep node 'ast:node-abstraction)
        (let ((type (ast:node-type node)))
          (when (ast:node-abstraction-keyword-params node)
            (unsupported "keyword arguments"))
          (make-binfo name node :function
                      (mapcar #'shape-of (tc:function-ty-positional-input-types type))
                      (shape-of (tc:output-types-result-type (tc:function-ty-output-types type)))))
        (make-binfo name node :value '() (shape-of (ast:node-type node))))))

(defstruct (analysis (:constructor %make-analysis))
  specialized shapes sites binfos lams global-lams group-lams)

(defun analyze (specialized)
  "Find the lambdas that reach every function-typed slot of SPECIALIZED."
  (let* ((*unified* (make-hash-table :test 'eq))
         (*owns-function* (make-hash-table :test 'eq))
         (*shapes* (make-hash-table :test 'eq))
         (*sites* (make-hash-table :test 'eq))
         (*binfos* (make-hash-table :test 'eq))
         (*lams* '())
         (*global-lams* (make-hash-table :test 'eq))
         (*group-lams* (make-hash-table :test 'eq))
         (*owner* nil)
         (*at* nil)
         (bindings (specialized-bindings specialized)))
    (dolist (scc (binding-sccs bindings))
      (dolist (name scc)
        (setf (gethash name *binfos*) (make-binfo-for name (gethash name bindings))))
      (dolist (name scc)
        (infer-binding (gethash name *binfos*)))
      (dolist (name scc)
        (setf (binfo-state (gethash name *binfos*)) :done)))
    (%make-analysis :specialized specialized :shapes *shapes* :sites *sites*
                    :binfos *binfos* :lams (reverse *lams*) :global-lams *global-lams*
                    :group-lams *group-lams*)))

;;; Concrete types
;;;
;;; Under an instance of a binding, each shape resolves to a ctype: the lambda
;;; sets, now fixed, and the types they make.

(defstruct (ctype (:constructor nil)))

(defstruct (atom-ctype (:include ctype) (:constructor make-atom-ctype (ty)))
  ty)

(defstruct (fn-ctype (:include ctype))
  "CASES pairs each lambda with the ctypes of its captures, in order of lambda."
  cases args ret ty)

(defstruct (data-ctype (:include ctype))
  source args ctors ty)

(defvar *mapping*)
(defvar *fn-memo*)
(defvar *data-memo*)

(defun materialize (shape)
  "The ctype of SHAPE under the current instance."
  (etypecase shape
    (atom-shape (make-atom-ctype (shape-ty shape)))
    (fn-shape
     (let ((root (find-cls (fn-shape-cls shape))))
       (or (gethash root *mapping*)
           (gethash root *fn-memo*)
           (let ((ctype (make-fn-ctype)))
             (setf (gethash root *fn-memo*) ctype
                   (fn-ctype-args ctype) (mapcar #'materialize (fn-shape-args shape))
                   (fn-ctype-ret ctype) (materialize (fn-shape-ret shape))
                   (fn-ctype-cases ctype)
                   (sort (loop :for lcase :in (cls-cases root)
                               :collect (cons (lcase-lam lcase) (mapcar #'materialize (lcase-captures lcase))))
                         #'< :key (lambda (entry) (lam-id (car entry)))))
             ctype))))
    (data-shape
     (let ((root (find-data shape)))
       (or (gethash root *data-memo*)
           (let ((ctype (make-data-ctype :source (shape-ty root))))
             (setf (gethash root *data-memo*) ctype
                   (data-ctype-args ctype) (mapcar #'materialize (data-shape-args root))
                   (data-ctype-ctors ctype) (loop :for (ctor . fields) :in (data-shape-ctors root)
                                                  :collect (cons ctor (mapcar #'materialize fields))))
             ctype))))))

(defvar *bound*)

(defun bind-ctype (shape ctype)
  "Fix the lambda sets of SHAPE to those of CTYPE for the current instance."
  (etypecase shape
    (atom-shape)
    (fn-shape
     (let ((root (find-cls (fn-shape-cls shape))))
       (unless (gethash root *mapping*)
         (setf (gethash root *mapping*) ctype)
         (mapc #'bind-ctype (fn-shape-args shape) (fn-ctype-args ctype))
         (bind-ctype (fn-shape-ret shape) (fn-ctype-ret ctype))
         (dolist (lcase (cls-cases root))
           (let ((entry (assoc (lcase-lam lcase) (fn-ctype-cases ctype))))
             (when entry
               (mapc #'bind-ctype (lcase-captures lcase) (cdr entry))))))))
    (data-shape
     (let ((root (find-data shape)))
       (unless (gethash root *bound*)
         (setf (gethash root *bound*) t
               (gethash root *data-memo*) ctype)
         (mapc #'bind-ctype (data-shape-args root) (data-ctype-args ctype))
         (loop :for (nil . fields) :in (data-shape-ctors root)
               :for (nil . ctype-fields) :in (data-ctype-ctors ctype)
               :do (mapc #'bind-ctype fields ctype-fields)))))))

(defun ctype-key (ctype deep &optional path)
  "A key equal for equal ctypes. DEEP also keys the arguments and results of
function types, which the lambda sets alone do not fix."
  ;; TODO: isomorphic cyclic graphs entered at different nodes key differently;
  ;; minimise by bisimulation before keying.
  ;; https://todo.sr.ht/~takeiteasy/sheaf/21
  (etypecase ctype
    (atom-ctype (type-key (atom-ctype-ty ctype)))
    (ctype
     (let ((position (position ctype path)))
       (if position
           (list :recur position)
           (let ((path (cons ctype path)))
             (flet ((keys (ctypes) (mapcar (lambda (c) (ctype-key c deep path)) ctypes)))
               (etypecase ctype
                 (fn-ctype
                  (list* :fn
                         (loop :for (lam . captures) :in (fn-ctype-cases ctype)
                               :collect (cons (lam-id lam) (keys captures)))
                         (and deep
                              (list (keys (fn-ctype-args ctype)) (car (keys (list (fn-ctype-ret ctype))))))))
                 (data-ctype
                  (list :data (type-key (data-ctype-source ctype))
                        (keys (data-ctype-args ctype))
                        (loop :for (ctor . fields) :in (data-ctype-ctors ctype)
                              :collect (cons ctor (keys fields)))))))))))))

;;; Synthesized types

(defstruct (tinfo (:constructor make-tinfo (name tycon)))
  name tycon (ctors '()))

(defvar *env*)
(defvar *generation* 0)
(defvar *generation-id*)
(defvar *types*)
(defvar *type-definitions*)
(defvar *fallback-location*)

(defun generated-symbol (control &rest args)
  "A symbol of the package Coalton's generated code looks type and constructor
names up in, unique to this run."
  (intern (format nil "~a.~d" (apply #'format nil control args) *generation-id*) '#:sheaf/generated))

(defun fn-type (args result)
  (tc:make-function-type* args result))

(defun define-type-info (info field-types location)
  "Register the type INFO in the environment, with one constructor per entry of
FIELD-TYPES, a list of the field types of each constructor."
  (let* ((name (tinfo-name info))
         (tycon (tinfo-tycon info))
         (location (or location *fallback-location*))
         (entries (loop :for ctor :in (tinfo-ctors info)
                        :for fields :in field-types
                        :collect (tc:make-constructor-entry
                                  :name ctor :source-name (symbol-name ctor)
                                  :arity (length fields) :constructs name
                                  :classname (intern (format nil "~a/~a" name ctor) '#:sheaf/generated)
                                  :docstring nil :compressed-repr nil)))
         (schemes (loop :for fields :in field-types
                        :collect (tc:to-scheme (tc:prepend-function-input-types fields tycon))))
         (definition (tc:make-type-definition
                      :name name :type tycon :runtime-type name :aliased-type nil
                      :explicit-repr nil :enum-repr nil :newtype nil
                      :constructors entries :constructor-types schemes
                      :constructor-args field-types :docstring nil :location location
                      :exception-p nil :resumption-p nil)))
    (setf *env* (tc:set-type *env* name
                             (tc:make-type-entry
                              :name name :source-name (symbol-name name) :runtime-type name
                              :type tycon :tyvars '() :variances '()
                              :constructors (tinfo-ctors info)
                              :explicit-repr nil :enum-repr nil :newtype nil
                              :docstring nil :location location
                              :exception-p nil :resumption-p nil)))
    (loop :for entry :in entries
          :for scheme :in schemes
          :for ctor := (tc:constructor-entry-name entry)
          :do (setf *env* (tc:set-constructor *env* ctor entry)
                    *env* (tc:set-value-type *env* ctor scheme)
                    *env* (tc:set-name *env* ctor (tc:make-name-entry
                                                  :name ctor :type :constructor
                                                  :docstring nil :location location)))
              (when (plusp (tc:constructor-entry-arity entry))
                (setf *env* (tc:set-function *env* ctor (tc:make-function-env-entry
                                                        :name ctor :arity (tc:constructor-entry-arity entry)
                                                        :inline-p nil)))))
    (push definition *type-definitions*)
    info))

(defun new-type-info (key base)
  (let* ((name (generated-symbol "~a~d" base (hash-table-count *types*)))
         (info (make-tinfo name (tc:make-tycon :name name :kind tc:+kstar+))))
    (setf (gethash key *types*) info)
    info))

(defun union-info (ctype)
  (let ((key (list :union (ctype-key ctype nil))))
    (or (gethash key *types*)
        (let ((info (new-type-info key "FN")))
          (setf (tinfo-ctors info)
                (loop :for (lam) :in (fn-ctype-cases ctype)
                      :collect (intern (format nil "~a/L~d" (tinfo-name info) (lam-id lam)) '#:sheaf/generated)))
          (define-type-info info
                            (loop :for (nil . captures) :in (fn-ctype-cases ctype)
                                  :collect (mapcar #'ctype-ty captures))
                            (loop :for (lam) :in (fn-ctype-cases ctype)
                                  :thereis (and (lam-node lam) (ast:node-location (lam-node lam)))))
          info))))

(defun data-head (ctype)
  (first (tc:flatten-type (data-ctype-source ctype))))

(defun owning-data-p (ctype)
  (tycon-owns-function-p (tc:tycon-name (data-head ctype))))

(defun data-info (ctype)
  "The synthesized type of a data type that holds functions itself."
  (let ((key (list :owned (ctype-key ctype nil))))
    (or (gethash key *types*)
        (let ((info (new-type-info key (symbol-name (tc:tycon-name (data-head ctype))))))
          (setf (tinfo-ctors info)
                (loop :for (ctor) :in (data-ctype-ctors ctype)
                      :collect (intern (format nil "~a/~a" (tinfo-name info) (symbol-name ctor)) '#:sheaf/generated)))
          (define-type-info info
                            (loop :for (nil . fields) :in (data-ctype-ctors ctype)
                                  :collect (mapcar #'ctype-ty fields))
                            nil)
          info))))

(defun ctype-ty (ctype)
  (etypecase ctype
    (atom-ctype (atom-ctype-ty ctype))
    (fn-ctype (or (fn-ctype-ty ctype)
                  (setf (fn-ctype-ty ctype) (tinfo-tycon (union-info ctype)))))
    (data-ctype
     (or (data-ctype-ty ctype)
         (setf (data-ctype-ty ctype)
               (if (owning-data-p ctype)
                   (tinfo-tycon (data-info ctype))
                   (tc:apply-type-argument-list (data-head ctype)
                                                (mapcar #'ctype-ty (data-ctype-args ctype)))))))))

(defun constructor-for (ctype constructor)
  "CONSTRUCTOR as the data type CTYPE names it."
  (if (and (data-ctype-p ctype) (owning-data-p ctype))
      (nth (position constructor (data-ctype-ctors ctype) :key #'car)
           (tinfo-ctors (data-info ctype)))
      constructor))

;;; Instances
;;;
;;; Every binding, and every lambda lifted out of one, is specialized once per
;;; combination of lambda sets it is used with.

(defstruct (inst (:constructor make-inst (kind target captures args ret symbol)))
  kind target captures args ret symbol)

(defvar *instances*)
(defvar *queue*)
(defvar *lam-context* nil
  "The lambda whose body is being rewritten.")

(defun request-instance (kind target captures args ret)
  "The symbol of the instance of TARGET for the given capture, argument and result
ctypes."
  (let ((key (list kind
                   (if (eq kind :lam) (lam-id target) (binfo-name target))
                   (loop :for c :in captures :collect (ctype-key c t))
                   (loop :for c :in args :collect (ctype-key c t))
                   (ctype-key ret t))))
    (or (car (gethash key *instances*))
        (let ((symbol (make-symbol (format nil "~a{~d}"
                                           (if (eq kind :lam)
                                               (format nil "~a/L~d" (car (lam-owner target)) (lam-id target))
                                               (binfo-name target))
                                           (hash-table-count *instances*)))))
          (setf (gethash key *instances*) (list symbol))
          (push (make-inst kind target captures args ret symbol) *queue*)
          symbol))))

(defun instance-type (captures args ret)
  (fn-type (mapcar #'ctype-ty (append captures args)) (ctype-ty ret)))

;;; Rewriting

(defun node-ctype (node)
  (materialize (gethash node *shapes*)))

(defun group-reference-p (name)
  (and *lam-context* (member name (lam-group *lam-context*))))

(defun local-ref (name ctype location)
  (ast:make-node-local-variable :type (ctype-ty ctype) :value name :location location))

(defun construct-case (ctype lam captures location)
  "The union value of CTYPE for LAM, built from the nodes CAPTURES."
  (let* ((info (union-info ctype))
         (position (position lam (fn-ctype-cases ctype) :key #'car))
         (constructor (nth position (tinfo-ctors info)))
         (ty (ctype-ty ctype)))
    (if captures
        (ast:make-node-direct-application
         :type ty :location location :properties '()
         :rator-type (fn-type (mapcar #'ast:node-type captures) ty)
         :rator constructor :rands captures :keyword-rands '())
        (ast:make-node-global-variable :type ty :value constructor :location location))))

(defun capture-ref (name ctype location)
  "A reference, inside a lifted body, to the captured or rebuilt variable NAME."
  (if (group-reference-p name)
      (reconstruct name ctype location)
      (local-ref name ctype location)))

(defun reconstruct (name ctype location)
  "The closure bound to NAME, a recursive binding, rebuilt from the captures of
the lambda being rewritten."
  (let* ((lam (gethash name *group-lams*))
         (entry (assoc lam (fn-ctype-cases ctype))))
    (unless entry
      (unsupported "the recursive binding ~s not reaching its own uses" name))
    (construct-case ctype lam
                    (loop :for free :in (lam-free lam)
                          :for capture :in (cdr entry)
                          :collect (local-ref free capture location))
                    location)))

(defun trivial-node-p (node)
  (typep node '(or ast:node-literal ast:node-variable)))

(defun with-bound (nodes function)
  "Call FUNCTION with a trivial node for each of NODES, binding those that are
not trivial around the result."
  (let ((bindings '())
        (refs '()))
    (dolist (node nodes)
      (if (trivial-node-p node)
          (push node refs)
          (let ((name (gensym "ARG")))
            (push (cons name node) bindings)
            (push (ast:make-node-local-variable :type (ast:node-type node) :value name
                                                :location (ast:node-location node))
                  refs))))
    (let ((body (funcall function (nreverse refs))))
      (dolist (binding bindings body)
        (setf body (ast:make-node-bind :type (ast:node-type body) :location (ast:node-location (cdr binding))
                                       :name (car binding) :expr (cdr binding) :body body))))))

(defun dispatch (fn ctype args location)
  "A call of the function value FN, a node of union type CTYPE, with the nodes ARGS.
FN is evaluated before ARGS, as in the call it replaces."
  (let* ((cases (fn-ctype-cases ctype))
         (result (fn-ctype-ret ctype))
         (union (ctype-ty ctype))
         (bind-fn-p (not (trivial-node-p fn))))
    (with-bound (if bind-fn-p (cons fn args) args)
      (lambda (refs)
        (let ((fn (if bind-fn-p (first refs) fn))
              (args (if bind-fn-p (rest refs) refs)))
          (labels ((call (lam captures cap-nodes)
                   (let ((symbol (if (lam-target lam)
                                     (request-instance :binding (gethash (lam-target lam) *binfos*)
                                                       '() (fn-ctype-args ctype) result)
                                     (request-instance :lam lam captures (fn-ctype-args ctype) result))))
                     (ast:make-node-direct-application
                      :type (ctype-ty result) :location location :properties '()
                      :rator-type (instance-type captures (fn-ctype-args ctype) result)
                      :rator symbol :rands (append cap-nodes args) :keyword-rands '())))
                 (branch (lam captures constructor)
                   (let ((names (loop :for nil :in captures :collect (gensym "CAPTURE"))))
                     (ast:make-match-branch
                      :pattern (pattern:make-pattern-constructor
                                :type union :name constructor
                                :patterns (loop :for name :in names
                                                :for capture :in captures
                                                :collect (pattern:make-pattern-var
                                                          :type (ctype-ty capture) :name name)))
                      :body (call lam captures
                                  (loop :for name :in names
                                        :for capture :in captures
                                        :collect (local-ref name capture location)))))))
            (if (and cases (null (rest cases)) (null (cdr (first cases))) (trivial-node-p fn))
                (call (car (first cases)) '() '())
                ;; A union no lambda reaches is dead code: a match with no branches.
                (ast:make-node-match
                 :type (ctype-ty result) :location location :expr fn
                 :branches (loop :for (lam . captures) :in cases
                                 :for constructor :in (tinfo-ctors (union-info ctype))
                                 :collect (branch lam captures constructor))))))))))

(defun rewrite-pattern (pattern)
  (let* ((ctype (materialize (gethash pattern *shapes*)))
         (ty (ctype-ty ctype)))
    (etypecase pattern
      (pattern:pattern-var (pattern:make-pattern-var :type ty :name (pattern:pattern-var-name pattern)))
      (pattern:pattern-wildcard (pattern:make-pattern-wildcard :type ty))
      (pattern:pattern-literal pattern)
      (pattern:pattern-binding
       (pattern:make-pattern-binding :type ty
                                     :var (rewrite-pattern (pattern:pattern-binding-var pattern))
                                     :pattern (rewrite-pattern (pattern:pattern-binding-pattern pattern))))
      (pattern:pattern-constructor
       (pattern:make-pattern-constructor
        :type ty
        :name (constructor-for ctype (pattern:pattern-constructor-name pattern))
        :patterns (mapcar #'rewrite-pattern (pattern:pattern-constructor-patterns pattern)))))))

(defun rewrite-variable (node)
  (let ((name (ast:node-variable-value node))
        (location (ast:node-location node)))
    (cond ((group-reference-p name)
           (reconstruct name (node-ctype node) location))
          ((gethash name *binfos*)
           (let ((binfo (gethash name *binfos*))
                 (ctype (node-ctype node)))
             (ecase (binfo-kind binfo)
               (:function (construct-case ctype (global-lam-of name) '() location))
               (:value (ast:make-node-global-variable
                        :type (ctype-ty ctype) :location location
                        :value (request-instance :binding binfo '() '() ctype))))))
          ((constructor-p name)
           (let ((ctype (node-ctype node)))
             (ast:make-node-global-variable :type (ctype-ty ctype) :location location
                                            :value (constructor-for ctype name))))
          (t (ast:copy-node node (ctype-ty (node-ctype node)))))))

(defun global-lam-of (name)
  (gethash name *global-lams*))

(defun rewrite-direct (node)
  (let* ((name (ast:node-direct-application-rator node))
         (site (gethash node *sites*))
         (rands (mapcar #'rewrite (ast:node-direct-application-rands node)))
         (ctype (node-ctype node))
         (location (ast:node-location node)))
    (cond ((fn-shape-p site)
           (let ((callee (materialize site)))
             (dispatch (if (group-reference-p name)
                           (reconstruct name callee location)
                           (local-ref name callee location))
                       callee rands location)))
          (site
           (rewrite-global-call node name (car site) (cdr site) rands ctype location))
          ((constructor-p name)
           (let ((ty (ctype-ty ctype)))
             (ast:make-node-direct-application
              :type ty :location location
              :properties (ast:node-direct-application-properties node)
              :rator-type (fn-type (mapcar #'ast:node-type rands) ty)
              :rator (constructor-for ctype name) :rands rands :keyword-rands '())))
          (t
           (ast:make-node-direct-application
            :type (ctype-ty ctype) :location location
            :properties (ast:node-direct-application-properties node)
            :rator-type (fn-type (mapcar #'ast:node-type rands) (ctype-ty ctype))
            :rator name :rands rands :keyword-rands '())))))

(defun global-instance (name params ret)
  "The instance symbol and type of the global NAME used with the shapes PARAMS and RET."
  (let ((args (mapcar #'materialize params))
        (result (materialize ret)))
    (values (request-instance :binding (gethash name *binfos*) '() args result)
            (instance-type '() args result))))

(defun rewrite-global-call (node name params ret rands ctype location)
  (declare (ignore node))
  (multiple-value-bind (symbol type) (global-instance name params ret)
    (ast:make-node-direct-application
     :type (ctype-ty ctype) :location location :properties '()
     :rator-type type :rator symbol :rands rands :keyword-rands '())))

(defun rewrite-application (node)
  (let* ((rator (ast:node-application-rator node))
         (site (gethash node *sites*))
         (rands (mapcar #'rewrite (ast:node-application-rands node)))
         (ctype (node-ctype node))
         (location (ast:node-location node))
         (name (and (typep rator 'ast:node-variable) (ast:node-variable-value rator))))
    (cond (site
           (multiple-value-bind (symbol type) (global-instance name (car site) (cdr site))
             (ast:make-node-application
              :type (ctype-ty ctype) :location location :properties '()
              :rator (ast:make-node-global-variable :type type :value symbol
                                                    :location (ast:node-location rator))
              :rands rands :keyword-rands '())))
          ((and name (not (gethash name *binfos*)) (constructor-p name)
                (not (gethash rator *shapes*)))
           (let ((ty (ctype-ty ctype)))
             (ast:make-node-application
              :type ty :location location :properties '()
              :rator (ast:make-node-global-variable
                      :type (fn-type (mapcar #'ast:node-type rands) ty)
                      :value (constructor-for ctype name) :location (ast:node-location rator))
              :rands rands :keyword-rands '())))
          (t
           (let ((fn (rewrite rator)))
             (dispatch fn (node-ctype rator) rands location))))))

(defun rewrite-lambda (node)
  (let* ((lam (gethash node *sites*))
         (ctype (node-ctype node))
         (captures (cdr (assoc lam (fn-ctype-cases ctype))))
         (location (ast:node-location node)))
    (construct-case ctype lam
                    (loop :for name :in (lam-free lam)
                          :for capture :in captures
                          :collect (capture-ref name capture location))
                    location)))

(defun rewrite (node)
  (let ((ty (lambda () (ctype-ty (node-ctype node))))
        (location (ast:node-location node)))
    (etypecase node
      (ast:node-literal node)
      (ast:node-variable (rewrite-variable node))
      (ast:node-application (rewrite-application node))
      (ast:node-direct-application (rewrite-direct node))
      (ast:node-abstraction (rewrite-lambda node))
      (ast:node-let
       (ast:make-node-let :type (funcall ty) :location location
                          :bindings (loop :for (name . value) :in (ast:node-let-bindings node)
                                          :collect (cons name (rewrite value)))
                          :subexpr (rewrite (ast:node-let-subexpr node))))
      (ast:node-bind
       (ast:make-node-bind :type (funcall ty) :location location
                           :name (ast:node-bind-name node)
                           :expr (rewrite (ast:node-bind-expr node))
                           :body (rewrite (ast:node-bind-body node))))
      (ast:node-seq
       (ast:make-node-seq :type (funcall ty) :location location
                          :nodes (mapcar #'rewrite (ast:node-seq-nodes node))))
      (ast:node-locally
       (ast:make-node-locally :type (funcall ty) :location location
                              :noinline-functions (ast:node-locally-noinline-functions node)
                              :type-check (ast:node-locally-type-check node)
                              :subexpr (rewrite (ast:node-locally-subexpr node))))
      (ast:node-match
       (ast:make-node-match
        :type (funcall ty) :location location
        :expr (rewrite (ast:node-match-expr node))
        :branches (loop :for branch :in (ast:node-match-branches node)
                        :collect (ast:make-match-branch
                                  :pattern (rewrite-pattern (ast:match-branch-pattern branch))
                                  :body (rewrite (ast:match-branch-body branch))))))
      (ast:node-lisp node))))

;;; Driver

(defun build-instance (request)
  "The IR of the instance REQUEST asks for."
  (let* ((*mapping* (make-hash-table :test 'eq))
         (*fn-memo* (make-hash-table :test 'eq))
         (*data-memo* (make-hash-table :test 'eq))
         (*bound* (make-hash-table :test 'eq))
         (target (inst-target request))
         (args (inst-args request))
         (ret (inst-ret request))
         (captures (inst-captures request)))
    (ecase (inst-kind request)
      (:binding
       (let* ((node (binfo-node target))
              (*owner* (cons (binfo-name target) (ast:node-type node)))
              (*lam-context* nil))
         (ecase (binfo-kind target)
           (:function
            (mapc #'bind-ctype (binfo-params target) args)
            (bind-ctype (binfo-ret target) ret)
            (values (ast:make-node-abstraction
                     :type (instance-type '() args ret) :location (ast:node-location node)
                     :vars (ast:node-abstraction-vars node) :keyword-params '()
                     :subexpr (rewrite (ast:node-abstraction-subexpr node)))
                    (binfo-name target)))
           (:value
            (bind-ctype (binfo-ret target) ret)
            (values (rewrite node) (binfo-name target))))))
      (:lam
       (let* ((node (lam-node target))
              (*owner* (cons (car (lam-owner target)) (ast:node-type node)))
              (*lam-context* target))
         (mapc #'bind-ctype (lam-free-shapes target) captures)
         (mapc #'bind-ctype (lam-params target) args)
         (bind-ctype (lam-ret target) ret)
         (values (ast:make-node-abstraction
                  :type (instance-type captures args ret) :location (ast:node-location node)
                  :vars (append (lam-free target) (ast:node-abstraction-vars node))
                  :keyword-params '()
                  :subexpr (rewrite (ast:node-abstraction-subexpr node)))
                 (car (lam-owner target))))))))

(defun defunctionalize (specialized)
  "SPECIALIZED with every function value replaced by a tagged union of the
lambdas that reach it, and every call through one by a match."
  (let* ((analysis (analyze specialized))
         (*shapes* (analysis-shapes analysis))
         (*sites* (analysis-sites analysis))
         (*binfos* (analysis-binfos analysis))
         (*global-lams* (analysis-global-lams analysis))
         (*group-lams* (analysis-group-lams analysis))
         (*env* (specialized-environment specialized))
         (*generation-id* (incf *generation*))
         (*types* (make-hash-table :test 'equal))
         (*type-definitions* '())
         (*instances* (make-hash-table :test 'equal))
         (*queue* '())
         (*mapping* (make-hash-table :test 'eq))
         (*fn-memo* (make-hash-table :test 'eq))
         (*data-memo* (make-hash-table :test 'eq))
         (*owns-function* (make-hash-table :test 'eq))
         (*lam-context* nil)
         (*owner* nil)
         (*at* nil)
         (root (gethash (specialized-root specialized) *binfos*))
         (*fallback-location* (ast:node-location (binfo-node root)))
         (result (%make-specialized)))
    (setf (specialized-root result)
          (request-instance :binding root '()
                            (mapcar #'materialize (binfo-params root))
                            (materialize (binfo-ret root))))
    (loop :for request := (pop *queue*)
          :while request
          :do (multiple-value-bind (node origin) (build-instance request)
                (setf (gethash (inst-symbol request) (specialized-bindings result)) node
                      (gethash (inst-symbol request) (specialized-origins result))
                      (list (or (specialized-origin specialized origin) origin) (ast:node-type node)))))
    (setf (specialized-environment result) *env*
          (specialized-type-definitions result) (reverse *type-definitions*))
    result))

(defun type-contains-function-p (type)
  (etypecase type
    (tc:function-ty t)
    (tc:tapp (or (type-contains-function-p (tc:tapp-from type))
                 (type-contains-function-p (tc:tapp-to type))))
    ((or tc:tycon tc:tyvar) nil)
    (tc:result-ty (some #'type-contains-function-p (tc:result-ty-output-types type)))))

(defun check-defunctionalized (specialized)
  "Signal an error unless SPECIALIZED passes `check-monomorphic` and holds no
function value: no lambda but each binding's own, and no function-typed node but
a global named in a call."
  (check-monomorphic specialized)
  (let ((entry:*global-environment* (specialized-environment specialized))
        (*owns-function* (make-hash-table :test 'eq)))
    (maphash
     (lambda (name root)
       (labels ((fail (control &rest args)
                  (error "~s: ~?" name control args))
                (check (node &optional allowed)
                  (when (and (not allowed)
                             (or (type-contains-function-p (ast:node-type node))
                                 (type-mentions-function-p (ast:node-type node))))
                    (fail "~a holds a function value" (tc:type-to-string (ast:node-type node))))
                  (typecase node
                    (ast:node-application
                     (check (ast:node-application-rator node)
                            (typep (ast:node-application-rator node) 'ast:node-global-variable))
                     (mapc #'check (ast:node-application-rands node)))
                    (ast:node-abstraction
                     (fail "a lambda is left"))
                    (t (mapc #'check (node-children node))))))
         (if (typep root 'ast:node-abstraction)
             (check (ast:node-abstraction-subexpr root))
             (check root))))
     (specialized-bindings specialized))
    t))

(defun recursive-types (specialized)
  "The names of the synthesized types of SPECIALIZED that contain themselves, which
no value of a fixed size can."
  (let ((fields (make-hash-table :test 'eq))
        (names '()))
    (dolist (definition (specialized-type-definitions specialized))
      (let ((name (tc:tycon-name (tc:type-definition-type definition))))
        (push name names)
        (setf (gethash name fields)
              (loop :for scheme :in (tc:type-definition-constructor-types definition)
                    :append (loop :for field :in (tc:function-type-arguments
                                                  (tc:qualified-ty-type (tc:fresh-inst scheme)))
                                  :collect (first (tc:flatten-type field)))))))
    (flet ((reaches-itself-p (name)
             (let ((seen '())
                   (stack (mapcar #'tc:tycon-name (remove-if-not #'tc:tycon-p (gethash name fields)))))
               (loop :while stack
                     :do (let ((next (pop stack)))
                           (when (eq next name) (return-from reaches-itself-p t))
                           (unless (member next seen)
                             (push next seen)
                             (setf stack (append (mapcar #'tc:tycon-name
                                                         (remove-if-not #'tc:tycon-p (gethash next fields)))
                                                 stack)))))
               nil)))
      (remove-if-not #'reaches-itself-p (nreverse names)))))
