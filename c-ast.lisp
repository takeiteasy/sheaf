;;;; c-ast.lisp
;;;; C as s-expressions, and a printer that adds parentheses only where precedence needs them.

(in-package #:sheaf)

;;; Expressions and types

(defparameter *binary-operators*
  '((:* "*" 13) (:+ "+" 12) (:- "-" 12) (:< "<" 10) (:> ">" 10) (:== "==" 9) (:!= "!=" 9) (:&& "&&" 5))
  "The operators that take two operands: the C text and the precedence of each.")

(defun float-negative-p (value)
  (and (not (float-features:float-nan-p value)) (minusp (float-sign value))))

(defun node-precedence (node)
  "How tightly NODE binds: 17 for a primary, down to 3 for `?:`."
  (case (first node)
    (:int (if (minusp (second node)) 15 17))
    (:float (if (float-negative-p (second node)) 15 17))
    ((:call :-> :dot :index :post++ :post-- :compound) 16)
    ((:neg :deref :addr :cast) 15)
    (:? 3)
    ((:name :string :init :sizeof :alignof :offsetof :ptr :struct) 17)
    (t (third (assoc (first node) *binary-operators*)))))

(defun escape-c-string (string)
  (with-output-to-string (out)
    (loop :for char :across string
          :do (case char
                (#\Newline (write-string "\\n" out))
                ((#\\ #\") (write-char #\\ out) (write-char char out))
                (t (write-char char out))))))

(defun float-text (value)
  "VALUE, a float, as an exact C constant: a hexadecimal float, or NAN or INFINITY."
  (cond ((float-features:float-nan-p value) "NAN")
        ((float-features:float-infinity-p value) (if (plusp value) "INFINITY" "-INFINITY"))
        (t (multiple-value-bind (mantissa exponent sign) (integer-decode-float value)
             (format nil "~:[~;-~]0x~xp~d~:[~;f~]" (minusp sign) mantissa exponent (typep value 'single-float))))))

(defun operand (node minimum)
  "NODE as text, in parentheses if it binds more loosely than MINIMUM."
  (if (< (node-precedence node) minimum)
      (format nil "(~a)" (c-text node))
      (c-text node)))

(defun c-text (node)
  "The C text of the expression or type NODE."
  (destructuring-bind (kind &rest args) node
    (ecase kind
      (:name (first args))
      (:int (format nil "~d" (first args)))
      (:float (float-text (first args)))
      (:string (format nil "\"~a\"" (escape-c-string (first args))))
      (:ptr (format nil "~a *" (c-text (first args))))
      (:struct (format nil "struct ~a" (first args)))
      (:call (format nil "~a(~{~a~^, ~})" (first args) (mapcar #'c-text (rest args))))
      (:-> (format nil "~a->~a" (operand (first args) 16) (second args)))
      (:dot (format nil "~a.~a" (operand (first args) 16) (second args)))
      (:index (format nil "~a[~a]" (operand (first args) 16) (c-text (second args))))
      ((:post++ :post--) (format nil "~a~a" (operand (first args) 16) (if (eq kind :post++) "++" "--")))
      (:neg (format nil "-~a" (operand (first args) 16)))
      (:deref (format nil "*~a" (operand (first args) 15)))
      (:addr (format nil "&~a" (operand (first args) 15)))
      (:cast (format nil "(~a)~a" (c-text (first args)) (operand (second args) 15)))
      (:? (format nil "~a ? ~a : ~a" (operand (first args) 4) (c-text (second args)) (operand (third args) 3)))
      (:init (format nil "{ ~{~a~^, ~} }" (mapcar #'c-text args)))
      (:compound (format nil "(~a){ ~{~a~^, ~} }" (c-text (first args))
                         (loop :for (designator value) :in (rest args)
                               :collect (if designator
                                            (format nil "~a = ~a" designator (c-text value))
                                            (c-text value)))))
      (:sizeof (format nil "sizeof(~a)" (c-text (first args))))
      (:alignof (format nil "_Alignof(~a)" (c-text (first args))))
      (:offsetof (format nil "offsetof(~a, ~a)" (c-text (first args)) (second args)))
      ((:* :+ :- :< :> :== :!= :&&)
       (destructuring-bind (text precedence) (rest (assoc kind *binary-operators*))
         (format nil "~a ~a ~a" (operand (first args) precedence) text (operand (second args) (1+ precedence))))))))

;;; Statements

(defun declaration-text (type name)
  (let ((text (c-text type)))
    (format nil "~a~:[ ~;~]~a" text (char= (char text (1- (length text))) #\*) name)))

(defun decl-text (node)
  "The declaration NODE without its semicolon."
  (destructuring-bind (type name init &key unused static array) (rest node)
    (format nil "~:[~;static ~]~a~:[~; SHEAF_UNUSED~]~@[ = ~a~]"
            static (declaration-text type (if array (format nil "~a[~d]" name array) name))
            unused (and init (c-text init)))))

(defun function-head (type name params &key public)
  "The head of the function NAME returning TYPE, with PARAMS as text, `void` if none."
  (format nil "~:[static SHEAF_UNUSED ~;~]~a" public
          (declaration-text type (format nil "~a(~:[void~;~:*~{~a~^, ~}~])" name params))))

(defun print-line (stream depth control &rest args)
  (dotimes (i depth) (write-string "  " stream))
  (apply #'format stream control args)
  (terpri stream))

(defun print-members (members stream depth)
  "The members of a struct or union."
  (dolist (member members)
    (destructuring-bind (kind &rest args) member
      (ecase kind
        (:field (print-line stream depth "~a;" (declaration-text (first args) (second args))))
        (:flexible (print-line stream depth "~a[];" (declaration-text (first args) (second args))))
        (:union
         (print-line stream depth "union {")
         (print-members (rest args) stream (1+ depth))
         (print-line stream depth "} ~a;" (first args)))
        (:anon-struct
         (destructuring-bind (name array-p &rest fields) args
           (print-line stream depth "struct {")
           (print-members fields stream (1+ depth))
           (print-line stream depth "} ~a~:[~;[]~];" name array-p)))))))

(defun print-if (node stream depth)
  "The `if` NODE, with an `if` as its else printed as `else if`."
  (let ((head "if"))
    (loop
      (destructuring-bind (test then &optional else) (rest node)
        (print-line stream depth "~a (~a) {" head (c-text test))
        (print-c then stream (1+ depth))
        (cond ((and else (null (rest else)) (eq (first (first else)) :if))
               (setf node (first else)
                     head "} else if"))
              (else
               (print-line stream depth "} else {")
               (print-c else stream (1+ depth))
               (print-line stream depth "}")
               (return))
              (t
               (print-line stream depth "}")
               (return)))))))

(defun print-c (nodes stream &optional (depth 0))
  "Write the statements NODES to STREAM, each on its own lines, indented DEPTH levels."
  (dolist (node nodes)
    (destructuring-bind (kind &rest args) node
      (ecase kind
        (:decl (print-line stream depth "~a;" (decl-text node)))
        (:assign (print-line stream depth "~a = ~a;" (c-text (first args)) (c-text (second args))))
        (:expr (print-line stream depth "~a;" (c-text (first args))))
        (:return (print-line stream depth "return ~a;" (c-text (first args))))
        (:goto (print-line stream depth "goto ~a;" (first args)))
        (:label (print-line stream depth "~a: ;" (first args)))
        (:comment (print-line stream depth "// ~a" (substitute #\Space #\Newline (first args))))
        (:blank (terpri stream))
        (:block
         (print-line stream depth "{")
         (print-c args stream (1+ depth))
         (print-line stream depth "}"))
        (:if (print-if node stream depth))
        (:include (print-line stream depth "#include \"~a\"" (first args)))
        (:struct-decl (print-line stream depth "struct ~a;" (first args)))
        (:struct-def
         (print-line stream depth "struct ~a {" (first args))
         (print-members (rest args) stream (1+ depth))
         (print-line stream depth "};"))
        (:proto
         (destructuring-bind (type name &rest types) args
           (print-line stream depth "~a;" (function-head type name (mapcar #'c-text types)))))
        (:fn
         (destructuring-bind (type name params body &key public) args
           (print-line stream depth "~a {"
                       (function-head type name
                                      (loop :for (param-type param-name) :in params
                                            :collect (format nil "~a SHEAF_UNUSED"
                                                             (declaration-text param-type param-name)))
                                      :public public))
           (print-c body stream (1+ depth))
           (print-line stream depth "}")))
        (:while
         (destructuring-bind (test &rest body) args
           (print-line stream depth "while (~a) {" (c-text test))
           (print-c body stream (1+ depth))
           (print-line stream depth "}")))
        (:for
         (destructuring-bind (init test step &rest body) args
           (print-line stream depth "for (~a; ~a; ~a) {" (decl-text init) (c-text test) (c-text step))
           (print-c body stream (1+ depth))
           (print-line stream depth "}")))))))
