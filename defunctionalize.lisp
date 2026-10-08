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
shapes of its fields."
  args ctors)

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
       (mapc #'unify-shapes (data-shape-args a) (data-shape-args b))
       (loop :for (nil . fields-a) :in (data-shape-ctors a)
             :for (nil . fields-b) :in (data-shape-ctors b)
             :do (mapc #'unify-shapes fields-a fields-b))))))

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
         (let ((new (make-data-shape :ty (shape-ty shape))))
           (setf (gethash shape memo) new
                 (data-shape-args new) (copy-shapes (data-shape-args shape) memo)
                 (data-shape-ctors new) (loop :for (ctor . fields) :in (data-shape-ctors shape)
                                              :collect (cons ctor (copy-shapes fields memo))))
           new)))))

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

(defun node-children (node)
  (etypecase node
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
                          (mapcar #'match-branch-body-of (ast:node-match-branches node))))))

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
  (etypecase node
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
    (ast:node-lisp (infer-lisp node env))))

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
          ((constructor-p name) (infer-constructor node '()))
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
                                 (setf (gethash (cdr binding) *shapes*) value
                                       (lam-self (lcase-lam (first (cls-cases (fn-shape-cls value)))))
                                       (car binding))
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
  specialized shapes sites binfos lams global-lams)

(defun analyze (specialized)
  "Find the lambdas that reach every function-typed slot of SPECIALIZED."
  (let* ((*unified* (make-hash-table :test 'eq))
         (*owns-function* (make-hash-table :test 'eq))
         (*shapes* (make-hash-table :test 'eq))
         (*sites* (make-hash-table :test 'eq))
         (*binfos* (make-hash-table :test 'eq))
         (*lams* '())
         (*global-lams* (make-hash-table :test 'eq))
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
                    :binfos *binfos* :lams (reverse *lams*) :global-lams *global-lams*)))
