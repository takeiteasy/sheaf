;;;; monomorphize.lisp
;;;; Specializing a captured program at ground types and erasing type-class
;;;; dictionaries.

(in-package #:sheaf)

(defparameter *specialization-limit* 16
  "How many specializations of one binding may be in progress at once.")

(define-condition specialization-error (error)
  ((message :initarg :message :reader specialization-error-message)
   (chain :initarg :chain :initform '() :reader specialization-error-chain
          :documentation "The requests leading to the error, outermost first,
as lists of a name, a type or a description of it, and a source location."))
  (:report (lambda (condition stream)
             (write-string (specialization-error-message condition) stream)
             (loop :for (name type location) :in (specialization-error-chain condition)
                   :do (format stream "~%  ~s at ~a~@[, offset ~d~]"
                               name (if (stringp type) type (tc:type-to-string type))
                               (and location (source:span-start (source:location-span location))))))))

(defun specialization-error (chain control &rest args)
  (error 'specialization-error :message (apply #'format nil control args) :chain chain))

(defstruct (specialized (:constructor %make-specialized))
  "A program specialized at ground types, with no type-class dictionaries."
  (bindings (make-hash-table :test 'eq) :type hash-table :read-only t)
  (origins (make-hash-table :test 'eq) :type hash-table :read-only t)
  (passthrough (make-hash-table :test 'eq) :type hash-table :read-only t)
  (root nil :type symbol)
  (environment entry:*global-environment* :type tc:environment)
  (type-definitions '() :type list))

(defun specialized-origin (specialized name)
  "The global the specialization NAME was made from, and its ground type."
  (values-list (gethash name (specialized-origins specialized))))

;;; Dictionaries

(defvar *classes*)

(defun class-table ()
  "Every class in the environment, by the codegen symbol its dictionaries carry."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (class (immutable-map:immutable-map-values
                    (tc:environment-class-environment entry:*global-environment*))
                   table)
      (setf (gethash (tc:ty-class-codegen-sym class) table) class))))

(defun dictionary-class (type)
  "The class TYPE is a dictionary of, or NIL."
  (let ((head (first (tc:flatten-type type))))
    (and (tc:tycon-p head)
         (values (gethash (tc:tycon-name head) *classes*)))))

(defun erase-dictionaries (type)
  "TYPE without dictionary arguments. A function of only dictionaries becomes
its result."
  (etypecase type
    (tc:function-ty
     (let* ((inputs (tc:function-ty-positional-input-types type))
            (kept (remove-if #'dictionary-class inputs))
            (keywords (tc:function-ty-keyword-input-types type))
            (outputs (mapcar #'erase-dictionaries (tc:function-ty-output-types type))))
       (if (and inputs (null kept) (null keywords))
           (tc:output-types-result-type outputs)
           (tc:make-function-ty
            :positional-input-types (mapcar #'erase-dictionaries kept)
            :keyword-input-types (mapcar (lambda (entry)
                                           (tc:make-keyword-ty-entry
                                            :keyword (tc:keyword-ty-entry-keyword entry)
                                            :type (erase-dictionaries (tc:keyword-ty-entry-type entry))))
                                         keywords)
            :keyword-open-p (tc:function-ty-keyword-open-p type)
            :output-types outputs))))
    (tc:tapp
     (tc:make-tapp :from (erase-dictionaries (tc:tapp-from type))
                   :to (erase-dictionaries (tc:tapp-to type))))
    (tc:result-ty
     (tc:make-result-ty :output-types (mapcar #'erase-dictionaries (tc:result-ty-output-types type))))
    (tc:ty type)))

(defun mentions-dictionary-p (type)
  (etypecase type
    (tc:function-ty
     (or (some #'mentions-dictionary-p (tc:function-ty-positional-input-types type))
         (some #'mentions-dictionary-p (mapcar #'tc:keyword-ty-entry-type
                                               (tc:function-ty-keyword-input-types type)))
         (some #'mentions-dictionary-p (tc:function-ty-output-types type))))
    (tc:result-ty (some #'mentions-dictionary-p (tc:result-ty-output-types type)))
    (tc:ty (or (dictionary-class type)
               (and (tc:tapp-p type)
                    (or (mentions-dictionary-p (tc:tapp-from type))
                        (mentions-dictionary-p (tc:tapp-to type))))))))

;;; Types

(defun type-key (type)
  "A key equal for equal types."
  (etypecase type
    (tc:tycon (tc:tycon-name type))
    (tc:tyvar (list :var (tc:tyvar-id type)))
    (tc:tapp (list :app (type-key (tc:tapp-from type)) (type-key (tc:tapp-to type))))
    (tc:function-ty
     (list :fn
           (mapcar #'type-key (tc:function-ty-positional-input-types type))
           (mapcar (lambda (entry)
                     (cons (tc:keyword-ty-entry-keyword entry)
                           (type-key (tc:keyword-ty-entry-type entry))))
                   (tc:function-ty-keyword-input-types type))
           (tc:function-ty-keyword-open-p type)
           (mapcar #'type-key (tc:function-ty-output-types type))))
    (tc:result-ty (cons :values (mapcar #'type-key (tc:result-ty-output-types type))))))

(defun ground-p (x)
  (null (tc:type-variables x)))

(defun method-p (name)
  (let ((entry (tc:lookup-name entry:*global-environment* name :no-error t)))
    (and entry (eq (tc:name-entry-type entry) :method))))

(defun constructor-p (name)
  (and (tc:lookup-constructor entry:*global-environment* name :no-error t) t))

(defun global-p (name)
  (and (tc:lookup-value-type entry:*global-environment* name :no-error t) t))

;;; Requests

(defstruct (request (:constructor make-request (name type symbol parent location)))
  "A global to specialize at a ground type, and the request that reached it."
  (name nil :type symbol :read-only t)
  (type nil :type tc:ty :read-only t)
  (symbol nil :type symbol :read-only t)
  (parent nil :type (or null request) :read-only t)
  (location nil :read-only t))

(defvar *program*)
(defvar *requests*)
(defvar *queue*)
(defvar *current* nil
  "The request being specialized.")
(defvar *current-location* nil
  "The location of the code being specialized, for references without one.")

(defun request-chain (request)
  (loop :for r := request :then (request-parent r)
        :while r
        :collect (list (request-name r) (request-type r) (request-location r)) :into chain
        :finally (return (nreverse chain))))

(defun check-recursion-limit (name chain)
  (when (> (count name chain :key #'first) *specialization-limit*)
    (specialization-error chain "~s has more than ~d specializations in progress: polymorphic recursion"
                          name *specialization-limit*)))

(defun instance-method (method type)
  "The instance method that METHOD, used at TYPE, resolves to."
  (let* ((dictionary (first (tc:function-ty-positional-input-types type)))
         (class (dictionary-class dictionary))
         (predicate (tc:make-ty-predicate :class (tc:ty-class-name class)
                                          :types (rest (tc:flatten-type dictionary))))
         (instance (or (tc:lookup-class-instance entry:*global-environment* predicate :no-error t)
                       (specialization-error (request-chain *current*) "No instance of ~a"
                                             (tc:type-to-string dictionary))))
         (index (position method (tc:ty-class-unqualified-methods class)
                          :key #'tc:ty-class-method-name)))
    (nth index (tc:ty-class-instance-method-codegen-syms instance))))

(defun specialization (name type location)
  "The symbol of NAME specialized where it is used at the ground TYPE."
  (let* ((code (or (binding-code *program* name)
                   (specialization-error (request-chain *current*) "~s has no code to specialize" name)))
         (generic (ast:node-type code))
         ;; The code's own dictionary parameters may differ from the use's, as
         ;; for a constrained instance's method, so unify the rest.
         (subs (tc:unify nil (erase-dictionaries generic) (erase-dictionaries type)))
         (ground (tc:apply-substitution subs generic))
         (key (cons name (type-key ground))))
    (unless (ground-p ground)
      (specialization-error (request-chain *current*) "~s at ~a leaves its type open"
                            name (tc:type-to-string ground)))
    (let ((request (gethash key *requests*)))
      (unless request
        (setf request (make-request name ground
                                    (make-symbol (format nil "~a<~a>" name
                                                         (tc:type-to-string (erase-dictionaries ground))))
                                    *current* (or location *current-location*)))
        (check-recursion-limit name (request-chain request))
        (setf (gethash key *requests*) request)
        (push request *queue*))
      (request-symbol request))))

(defun resolve-reference (name type location)
  "The specialization a reference to NAME at TYPE becomes, or NIL to keep it."
  (cond ((not (global-p name)) nil)
        ((dictionary-class type) nil)
        ((constructor-p name) nil)
        ((method-p name) (specialization (instance-method name type) type location))
        ((binding-code *program* name) (specialization name type location))
        ((mentions-dictionary-p type)
         (specialization-error (request-chain *current*) "~s takes dictionaries but has no code" name))))

;;; Local bindings

(defun non-expansive-p (node)
  "Whether NODE is one of the forms Coalton generalizes a local binding over."
  (typecase node
    ((or ast:node-abstraction ast:node-literal ast:node-local-variable ast:node-global-variable) t)
    (ast:node-application
     (let ((rator (ast:node-application-rator node)))
       (and (typep rator '(or ast:node-local-variable ast:node-global-variable))
            (constructor-p (ast:node-variable-value rator))
            (every #'non-expansive-p (ast:node-application-rands node)))))))

(defun local-references (node names)
  "The (name . type) of each reference in NODE to one of NAMES."
  (let ((refs '()))
    (traverse:traverse
     node
     (list
      (traverse:action (:after ast:node-local-variable n)
        (when (member (ast:node-variable-value n) names)
          (push (cons (ast:node-variable-value n) (ast:node-type n)) refs))
        (values))
      (traverse:action (:after ast:node-direct-application n)
        (when (member (ast:node-direct-application-rator n) names)
          (push (cons (ast:node-direct-application-rator n) (ast:node-direct-application-rator-type n))
                refs))
        (values))
      (traverse:action (:after ast:node-lisp n)
        (dolist (var (ast:node-lisp-vars n))
          (when (member (cdr var) names)
            (specialization-error (request-chain *current*)
                                  "Raw Lisp refers to the generalized local ~s" (cdr var))))
        (values))))
    refs))

(defun rename-local-references (node clones)
  "NODE with each reference to a cloned binding renamed to the clone for its type.
CLONES holds (name type-key new-name value) lists."
  (flet ((clone-name (name type)
           (third (find-if (lambda (clone)
                             (and (eq name (first clone)) (equal (type-key type) (second clone))))
                           clones))))
    (traverse:traverse
     node
     (list
      (traverse:action (:after ast:node-local-variable n)
        (let ((new (clone-name (ast:node-variable-value n) (ast:node-type n))))
          (when new
            (ast:make-node-local-variable :type (ast:node-type n) :value new
                                          :location (ast:node-location n)))))
      (traverse:action (:after ast:node-direct-application n)
        (let ((new (clone-name (ast:node-direct-application-rator n)
                               (ast:node-direct-application-rator-type n))))
          (when new
            (ast:make-node-direct-application
             :type (ast:node-type n)
             :location (ast:node-location n)
             :properties (ast:node-direct-application-properties n)
             :rator-type (ast:node-direct-application-rator-type n)
             :rator new
             :rands (ast:node-direct-application-rands n)
             :keyword-rands (ast:node-direct-application-keyword-rands n)))))))))

(defun check-open-bindings (bindings nodes)
  "Signal an error if one of BINDINGS that keeps an open type is used at
several types in NODES; only a binding Sheaf clones may be."
  (let ((open (loop :for (name . value) :in bindings
                    :unless (ground-p (ast:node-type value)) :collect name)))
    (when open
      (let ((uses (remove-duplicates
                   (loop :for node :in nodes
                         :append (mapcar (lambda (ref) (cons (car ref) (type-key (cdr ref))))
                                         (local-references node open)))
                   :test #'equal)))
        (dolist (name open)
          (when (> (count name uses :key #'car) 1)
            (specialization-error (request-chain *current*)
                                  "The local ~s is used at several types but is not a form Sheaf clones"
                                  name)))))))

(defun instantiate-bindings (bindings body)
  "Clone each generalized binding of BINDINGS, (name . value) pairs, once per
type it is used at in BODY and the other bindings. Returns the new bindings and
the renamed BODY; a generalized binding nothing uses is dropped."
  (let* ((generic (remove-if-not (lambda (binding)
                                   (and (not (ground-p (ast:node-type (cdr binding))))
                                        (non-expansive-p (cdr binding))))
                                 bindings))
         (names (mapcar #'car generic))
         (kept (set-difference bindings generic))
         (clones '())
         (pending (cons body (mapcar #'cdr kept))))
    (unless generic
      (check-open-bindings bindings (cons body (mapcar #'cdr bindings)))
      (return-from instantiate-bindings (values bindings body)))
    (loop :while pending
          :do (loop :for (name . type) :in (local-references (pop pending) names)
                    :for key := (type-key type)
                    :unless (find-if (lambda (clone)
                                       (and (eq name (first clone)) (equal key (second clone))))
                                     clones)
                      :do (when (>= (count name clones :key #'first) *specialization-limit*)
                            (specialization-error (request-chain *current*)
                                                  "The local ~s has more than ~d specializations: polymorphic recursion"
                                                  name *specialization-limit*))
                          (let* ((value (cdr (assoc name generic)))
                                 (clone (tc:apply-substitution
                                         (tc:unify nil (ast:node-type value) type)
                                         value)))
                            (push (list name key (gensym (symbol-name name)) clone) clones)
                            (push clone pending))))
    (check-open-bindings kept (list* body (append (mapcar #'cdr kept) (mapcar #'fourth clones))))
    (flet ((rename (node) (rename-local-references node clones)))
      (values (append (loop :for (name . value) :in kept
                            :collect (cons name (rename value)))
                      (loop :for (nil nil new-name value) :in (reverse clones)
                            :collect (cons new-name (rename value))))
              (rename body)))))

(defun instantiate-local-bindings (node)
  "NODE with each generalized local binding cloned once per type it is used at."
  (traverse:traverse
   node
   (list
    (traverse:action (:after ast:node-let n)
      (multiple-value-bind (bindings body)
          (instantiate-bindings (ast:node-let-bindings n) (ast:node-let-subexpr n))
        (cond ((null bindings) body)
              ((and (eq bindings (ast:node-let-bindings n)) (eq body (ast:node-let-subexpr n))) nil)
              (t (ast:make-node-let :type (ast:node-type n) :location (ast:node-location n)
                                    :bindings bindings :subexpr body)))))
    (traverse:action (:after ast:node-bind n)
      (multiple-value-bind (bindings body)
          (instantiate-bindings (list (cons (ast:node-bind-name n) (ast:node-bind-expr n)))
                                (ast:node-bind-body n))
        (unless (and (eq (cdr (first bindings)) (ast:node-bind-expr n)) (null (rest bindings)))
          (reduce (lambda (binding body)
                    (ast:make-node-bind :type (ast:node-type n) :location (ast:node-location n)
                                        :name (car binding) :expr (cdr binding) :body body))
                  bindings :from-end t :initial-value body)))))))

;;; Defaulting

(defun default-type-variables (node)
  "NODE with each type variable left open set to Unit."
  (let ((vars (tc:type-variables node)))
    (if (null vars)
        node
        (tc:apply-substitution
         (mapcar (lambda (var)
                   (unless (tc:kstar-p (tc:tyvar-kind var))
                     (specialization-error (request-chain *current*)
                                           "The type variable ~a of kind ~a is ambiguous"
                                           (tc:type-to-string var) (tc:tyvar-kind var)))
                   (tc:make-substitution :from var :to tc:*unit-type*))
                 vars)
         node))))

;;; Globals

(defun resolve-globals (node)
  "NODE with each reference to a global with code, or to a class method, made a
reference to a specialization."
  (flet ((resolve-variable (n)
           (let ((symbol (resolve-reference (ast:node-variable-value n) (ast:node-type n)
                                            (ast:node-location n))))
             (when symbol
               (ast:make-node-global-variable :type (ast:node-type n) :value symbol
                                              :location (ast:node-location n))))))
    (traverse:traverse
     node
     (list
      (traverse:action (:after ast:node-local-variable n) (resolve-variable n))
      (traverse:action (:after ast:node-global-variable n) (resolve-variable n))
      (traverse:action (:after ast:node-direct-application n)
        (let ((symbol (resolve-reference (ast:node-direct-application-rator n)
                                         (ast:node-direct-application-rator-type n)
                                         (ast:node-location n))))
          (when symbol
            (ast:make-node-direct-application
             :type (ast:node-type n)
             :location (ast:node-location n)
             :properties (ast:node-direct-application-properties n)
             :rator-type (ast:node-direct-application-rator-type n)
             :rator symbol
             :rands (ast:node-direct-application-rands n)
             :keyword-rands (ast:node-direct-application-keyword-rands n)))))))))

;;; Erasure

(defun dictionary-binders (node)
  "The local names NODE binds to dictionaries."
  (let ((names '()))
    (traverse:traverse
     node
     (list
      (traverse:action (:after ast:node-abstraction n)
        (loop :for var :in (ast:node-abstraction-vars n)
              :for type :in (tc:function-ty-positional-input-types (ast:node-type n))
              :when (dictionary-class type) :do (push var names))
        (values))
      (traverse:action (:after ast:node-bind n)
        (when (dictionary-class (ast:node-type (ast:node-bind-expr n)))
          (push (ast:node-bind-name n) names))
        (values))
      (traverse:action (:after ast:node-let n)
        (loop :for (name . value) :in (ast:node-let-bindings n)
              :when (dictionary-class (ast:node-type value)) :do (push name names))
        (values))))
    names))

(defun erase-dictionary-nodes (node)
  "NODE without dictionary parameters, arguments or bindings, and with
dictionary arguments removed from every type."
  (let ((binders (dictionary-binders node)))
    (flet ((dictionary-node-p (n) (dictionary-class (ast:node-type n))))
      (traverse:traverse
       node
       (list
        (traverse:action (:after ast:node-abstraction n)
          (let* ((vars (ast:node-abstraction-vars n))
                 (kept (loop :for var :in vars
                             :for type :in (tc:function-ty-positional-input-types (ast:node-type n))
                             :unless (dictionary-class type) :collect var)))
            (cond ((= (length kept) (length vars)) nil)
                  ((and (null kept) (null (ast:node-abstraction-keyword-params n)))
                   (ast:node-abstraction-subexpr n))
                  (t (ast:make-node-abstraction
                      :type (ast:node-type n) :location (ast:node-location n)
                      :vars kept
                      :keyword-params (ast:node-abstraction-keyword-params n)
                      :subexpr (ast:node-abstraction-subexpr n))))))
        (traverse:action (:after ast:node-application n)
          (let* ((rands (ast:node-application-rands n))
                 (kept (remove-if #'dictionary-node-p rands)))
            (cond ((= (length kept) (length rands)) nil)
                  ((and (null kept) (null (ast:node-application-keyword-rands n)))
                   (ast:node-application-rator n))
                  (t (ast:make-node-application
                      :type (ast:node-type n) :location (ast:node-location n)
                      :properties (ast:node-application-properties n)
                      :rator (ast:node-application-rator n)
                      :rands kept
                      :keyword-rands (ast:node-application-keyword-rands n))))))
        (traverse:action (:after ast:node-direct-application n)
          (let* ((rands (ast:node-direct-application-rands n))
                 (kept (remove-if #'dictionary-node-p rands))
                 (rator-type (erase-dictionaries (ast:node-direct-application-rator-type n))))
            (if (and rands (null kept) (null (ast:node-direct-application-keyword-rands n)))
                (ast:make-node-global-variable
                 :type rator-type :location (ast:node-location n)
                 :value (ast:node-direct-application-rator n))
                (ast:make-node-direct-application
                 :type (ast:node-type n) :location (ast:node-location n)
                 :properties (ast:node-direct-application-properties n)
                 :rator-type rator-type
                 :rator (ast:node-direct-application-rator n)
                 :rands kept
                 :keyword-rands (ast:node-direct-application-keyword-rands n)))))
        (traverse:action (:after ast:node-bind n)
          (when (dictionary-node-p (ast:node-bind-expr n))
            (ast:node-bind-body n)))
        (traverse:action (:after ast:node-let n)
          (let* ((bindings (ast:node-let-bindings n))
                 (kept (remove-if #'dictionary-node-p bindings :key #'cdr)))
            (cond ((= (length kept) (length bindings)) nil)
                  ((null kept) (ast:node-let-subexpr n))
                  (t (ast:make-node-let :type (ast:node-type n) :location (ast:node-location n)
                                        :bindings kept :subexpr (ast:node-let-subexpr n))))))
        (traverse:action (:after ast:node-lisp n)
          (dolist (var (ast:node-lisp-vars n))
            (when (member (cdr var) binders)
              ;; TODO: only optimized stdlib code passes dictionaries to raw
              ;; Lisp; capturing the stdlib removes this case.
              ;; https://github.com/takeiteasy/sheaf/issues/3
              (specialization-error (request-chain *current*)
                                    "Raw Lisp uses the dictionary ~s" (cdr var))))
          (values))
        (traverse:action (:after ast:node n)
          (ast:copy-node n (erase-dictionaries (ast:node-type n)))))))))

;;; Driver

(defun specialize (request)
  (let* ((*current* request)
         (code (binding-code *program* (request-name request)))
         (*current-location* (ast:node-location code))
         (node (tc:apply-substitution (tc:unify nil (ast:node-type code) (request-type request)) code)))
    (erase-dictionary-nodes
     (resolve-globals
      (default-type-variables
       (instantiate-local-bindings node))))))

(defun monomorphize (program root)
  "Specialize the globals ROOT reaches at the ground types they are used at,
and erase type-class dictionaries. ROOT must have a ground type."
  (let* ((*program* program)
         (*classes* (class-table))
         (*requests* (make-hash-table :test 'equal))
         (*queue* '())
         (specialized (%make-specialized))
         (code (or (binding-code program root)
                   (specialization-error '() "~s has no code" root))))
    (unless (ground-p (ast:node-type code))
      (specialization-error '() "~s has the open type ~a" root (tc:type-to-string (ast:node-type code))))
    (setf (specialized-root specialized)
          (specialization root (ast:node-type code) (ast:node-location code)))
    (loop :for request := (pop *queue*)
          :while request
          :do (setf (gethash (request-symbol request) (specialized-bindings specialized))
                    (specialize request)
                    (gethash (request-symbol request) (specialized-origins specialized))
                    (list (request-name request) (request-type request))))
    specialized))

(defun check-monomorphic (specialized)
  "Signal an error unless every binding of SPECIALIZED is ground, holds no
dictionary or class method, and passes Coalton's node typechecker."
  (let ((*classes* (class-table))
        (env (specialized-environment specialized)))
    (maphash
     (lambda (name node)
       (flet ((fail (control &rest args)
                (error "~s: ~?" name control args)))
         (unless (ground-p node)
           (fail "type variables ~{~a~^, ~} are left" (mapcar #'tc:type-to-string (tc:type-variables node))))
         (traverse:traverse
          node
          (list
           (traverse:action (:after ast:node n)
             (when (mentions-dictionary-p (ast:node-type n))
               (fail "~a has a dictionary type" (tc:type-to-string (ast:node-type n))))
             (values))
           (traverse:action (:after ast:node-global-variable n)
             (when (method-p (ast:node-variable-value n))
               (fail "the class method ~s is left" (ast:node-variable-value n)))
             (values))
           (traverse:action (:after ast:node-local-variable n)
             (when (method-p (ast:node-variable-value n))
               (fail "the class method ~s is left" (ast:node-variable-value n)))
             (values))))
         (typecheck:typecheck-node node env)))
     (specialized-bindings specialized))
    t))
