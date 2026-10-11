;;;; c-ast.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defun c-text (node)
  (sheaf::c-text node))

(defun c-lines (&rest nodes)
  (with-output-to-string (stream)
    (sheaf::print-c nodes stream)))

(defparameter *a* '(:name "a"))
(defparameter *b* '(:name "b"))
(defparameter *c* '(:name "c"))

(test binary-operators-are-bracketed-by-precedence
  (is (string= "(a + b) * c" (c-text `(:* (:+ ,*a* ,*b*) ,*c*))))
  (is (string= "a + b * c" (c-text `(:+ ,*a* (:* ,*b* ,*c*)))))
  (is (string= "a - b - c" (c-text `(:- (:- ,*a* ,*b*) ,*c*))))
  (is (string= "a - (b - c)" (c-text `(:- ,*a* (:- ,*b* ,*c*)))))
  (is (string= "a < b && b == c" (c-text `(:&& (:< ,*a* ,*b*) (:== ,*b* ,*c*))))))

(test unary-and-postfix-operators-bind-to-their-operand
  (is (string= "*a->b" (c-text `(:deref (:-> ,*a* "b")))))
  (is (string= "(*a)->b" (c-text `(:-> (:deref ,*a*) "b"))))
  (is (string= "&a[b]" (c-text `(:addr (:index ,*a* ,*b*)))))
  (is (string= "(a + b)[c]" (c-text `(:index (:+ ,*a* ,*b*) ,*c*))))
  (is (string= "f(a, b)->c" (c-text `(:-> (:call "f" ,*a* ,*b*) "c"))))
  (is (string= "a--" (c-text `(:post-- ,*a*)))))

(test negative-numbers-do-not-run-into-their-operator
  (is (string= "a - -5" (c-text `(:- ,*a* (:int -5)))))
  (is (string= "-(-5)" (c-text '(:neg (:int -5)))))
  (is (string= "-INT64_C(9223372036854775807) - 1"
               (c-text '(:- (:neg (:call "INT64_C" (:int 9223372036854775807))) (:int 1)))))
  (is (string= "(-INT64_C(1) - 1) * a"
               (c-text `(:* (:- (:neg (:call "INT64_C" (:int 1))) (:int 1)) ,*a*)))))

(test casts-and-pointer-types
  (is (string= "(struct A0 *)a" (c-text `(:cast (:ptr (:struct "A0")) ,*a*))))
  (is (string= "(uint8_t)(a + b)" (c-text `(:cast (:name "uint8_t") (:+ ,*a* ,*b*)))))
  (is (string= "(char *)a->e" (c-text `(:cast (:ptr (:name "char")) (:-> ,*a* "e")))))
  (is (string= "sizeof(((struct H0 *)0)->e[0])"
               (c-text '(:sizeof (:index (:-> (:cast (:ptr (:struct "H0")) (:int 0)) "e") (:int 0)))))))

(test a-conditional-nests-to-the-right
  (is (string= "a < b ? 0 : a > b ? 2 : 1"
               (c-text `(:? (:< ,*a* ,*b*) (:int 0) (:? (:> ,*a* ,*b*) (:int 2) (:int 1))))))
  (is (string= "(a ? b : c) ? a : b" (c-text `(:? (:? ,*a* ,*b* ,*c*) ,*a* ,*b*)))))

(test compound-literals-print-their-designators
  (is (string= "(struct T0){ .tag = 1, .u.c1 = { a, b } }"
               (c-text `(:compound (:struct "T0") (".tag" (:int 1)) (".u.c1" (:init ,*a* ,*b*))))))
  (is (string= "(struct S0){ a + b, c - b }"
               (c-text `(:compound (:struct "S0") (nil (:+ ,*a* ,*b*)) (nil (:- ,*c* ,*b*)))))))

(test floats-are-exact-constants
  (is (string= "0xC00000p-23f" (c-text '(:float 1.5f0))))
  (is (string= "-0xC00000p-23f" (c-text '(:float -1.5f0))))
  (is (string= "INFINITY" (c-text `(:float ,float-features:double-float-positive-infinity))))
  (is (string= "-INFINITY" (c-text `(:float ,float-features:double-float-negative-infinity))))
  (is (string= "a - -0xC00000p-23f" (c-text `(:- ,*a* (:float -1.5f0))))))

(test strings-are-escaped
  (is (string= "\"%llu\\n\"" (c-text (list :string (format nil "%llu~%"))))))

(test declarations-print-qualifiers-and-pointer-spacing
  (is (string= (format nil "uint64_t a;~%") (c-lines '(:decl (:name "uint64_t") "a" nil))))
  (is (string= (format nil "struct A0 *a = b;~%")
               (c-lines `(:decl (:ptr (:struct "A0")) "a" ,*b*))))
  (is (string= (format nil "static int ready;~%") (c-lines '(:decl (:name "int") "ready" nil :static t))))
  (is (string= (format nil "uint64_t a SHEAF_UNUSED = b;~%")
               (c-lines `(:decl (:name "uint64_t") "a" ,*b* :unused t)))))

(test an-else-if-chain-is-flat
  (is (string= (format nil "if (a) {~%  b = c;~%} else if (b) {~%  return c;~%} else {~%  goto top;~%}~%")
               (c-lines `(:if ,*a* ((:assign ,*b* ,*c*))
                              ((:if ,*b* ((:return ,*c*))
                                    ((:goto "top")))))))))

(test an-if-nested-in-a-then-keeps-its-block
  (is (string= (format nil "if (a) {~%  if (b) {~%    return c;~%  }~%}~%")
               (c-lines `(:if ,*a* ((:if ,*b* ((:return ,*c*)))))))))

(test blocks-loops-labels-and-comments-indent
  (is (string= (format nil "// two~%top: ;~%{~%  for (uint64_t i = UINT64_C(0); i < a; i++) {~%    a[i] = b;~%  }~%}~%")
               (c-lines '(:comment "two")
                        '(:label "top")
                        `(:block (:for (:decl (:name "uint64_t") "i" (:call "UINT64_C" (:int 0)))
                                       (:< (:name "i") ,*a*)
                                       (:post++ (:name "i"))
                                       (:assign (:index ,*a* (:name "i")) ,*b*)))))))

(test structs-print-their-members
  (is (string= (format nil "struct T1 {~%  uint32_t tag;~%  union {~%    struct {~%      uint64_t f0;~%      struct T1 *f1;~%    } c0;~%  } u;~%};~%")
               (c-lines '(:struct-def "T1" (:field (:name "uint32_t") "tag")
                          (:union "u" (:anon-struct "c0" nil
                                       (:field (:name "uint64_t") "f0")
                                       (:field (:ptr (:struct "T1")) "f1")))))))
  (is (string= (format nil "struct H0 {~%  sheaf_table table;~%  struct {~%    uint64_t key;~%  } e[];~%};~%")
               (c-lines '(:struct-def "H0" (:field (:name "sheaf_table") "table")
                          (:anon-struct "e" t (:field (:name "uint64_t") "key"))))))
  (is (string= (format nil "struct A0 {~%  uint64_t length;~%  uint8_t e[];~%};~%")
               (c-lines '(:struct-def "A0" (:field (:name "uint64_t") "length")
                          (:flexible (:name "uint8_t") "e")))))
  (is (string= (format nil "struct T0;~%") (c-lines '(:struct-decl "T0")))))

(test functions-print-their-heads
  (is (string= (format nil "static SHEAF_UNUSED struct T1 *f(uint64_t, struct T1 *);~%")
               (c-lines '(:proto (:ptr (:struct "T1")) "f" (:name "uint64_t") (:ptr (:struct "T1"))))))
  (is (string= (format nil "static SHEAF_UNUSED uint64_t g(void);~%")
               (c-lines '(:proto (:name "uint64_t") "g"))))
  (is (string= (format nil "static SHEAF_UNUSED uint64_t f(uint64_t n SHEAF_UNUSED) {~%  return n;~%}~%")
               (c-lines '(:fn (:name "uint64_t") "f" (((:name "uint64_t") "n")) ((:return (:name "n")))))))
  (is (string= (format nil "int main(void) {~%  return 0;~%}~%")
               (c-lines '(:fn (:name "int") "main" () ((:return (:int 0))) :public t))))
  (is (string= (format nil "#include \"sheaf.h\"~%")
               (c-lines '(:include "sheaf.h"))))
  (is (string= (format nil "static sheaf_bank banks[4] SHEAF_UNUSED;~%")
               (c-lines '(:decl (:name "sheaf_bank") "banks" nil :static t :array 4 :unused t)))))
