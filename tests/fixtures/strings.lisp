;;;; strings.lisp
;;;; Char and String: literals, comparison, and the core library.

(defpackage #:sheaf/fixtures/strings
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:char #:coalton/char)
   (#:iter #:coalton/iterator)
   (#:string #:coalton/string)))

(in-package #:sheaf/fixtures/strings)

(coalton-toplevel
  (declare flag (Boolean -> UFix))
  (define (flag b)
    (if b 1 0))

  (declare main-char-code (Void -> UFix))
  (define (main-char-code)
    (char:char-code #\A))

  (declare main-char-order (Void -> UFix))
  (define (main-char-order)
    (+ (flag (< #\a #\b)) (+ (* 2 (flag (== #\x #\x))) (* 4 (flag (> #\a #\b))))))

  (declare main-code-char (Void -> UFix))
  (define (main-code-char)
    (+ (match (char:code-char 233)
         ((Some c) (char:char-code c))
         ((None) 0))
       (match (char:code-char 66)
         ((Some c) (char:char-code c))
         ((None) 0))))

  (declare main-ascii (Void -> UFix))
  (define (main-ascii)
    (+ (flag (char:ascii-alpha? #\q))
       (+ (* 2 (flag (char:ascii-digit? #\7)))
          (+ (* 4 (flag (char:ascii-uppercase? #\Q)))
             (+ (* 8 (flag (char:ascii-lowercase? #\Q)))
                (+ (* 16 (flag (char:ascii-alphanumeric? #\_)))
                   (match (char:digit->ufix #\7)
                     ((Some n) (* 32 n))
                     ((None) 0))))))))

  (declare main-length (Void -> UFix))
  (define (main-length)
    (+ (string:length "hello") (* 10 (string:length "héllo 世界"))))

  (declare main-empty (Void -> UFix))
  (define (main-empty)
    (string:length ""))

  (declare main-concat (Void -> UFix))
  (define (main-concat)
    (string:length (string:concat "ab" (string:concat "cde" "é"))))

  (declare main-semigroup (Void -> UFix))
  (define (main-semigroup)
    (string:length (<> "ab" (<> "cd" mempty))))

  (declare main-substring (Void -> UFix))
  (define (main-substring)
    (+ (string:length (string:substring "hello world" 6 11))
       (+ (* 10 (string:length (string:substring "hello" 4 2)))
          (* 100 (string:length (string:substring "hello" 3 99))))))

  (declare main-ref (Void -> UFix))
  (define (main-ref)
    (+ (char:char-code (string:ref-unchecked "aéb" 1))
       (match (string:ref "xyz" 2)
         ((Some c) (char:char-code c))
         ((None) 0))))

  (declare main-ref-none (Void -> UFix))
  (define (main-ref-none)
    (match (string:ref "xyz" 3)
      ((Some _) 1)
      ((None) 2)))

  (declare main-equal (Void -> UFix))
  (define (main-equal)
    (+ (flag (== "abc" "abc"))
       (+ (* 2 (flag (== "abc" "abd")))
          (+ (* 4 (flag (== "" "")))
             (* 8 (flag (== "ab" "abc")))))))

  (declare main-order (Void -> UFix))
  (define (main-order)
    (+ (flag (< "abc" "abd"))
       (+ (* 2 (flag (> "abc" "abd")))
          (+ (* 4 (flag (< "ab" "abc")))
             (+ (* 8 (flag (< "é" "z")))
                (* 16 (flag (<= "same" "same"))))))))

  (declare main-prefix (Void -> UFix))
  (define (main-prefix)
    (+ (match (string:strip-prefix "he" "hello")
         ((Some rest) (string:length rest))
         ((None) 0))
       (match (string:strip-suffix "xx" "hello")
         ((Some _) 10)
         ((None) 20))))

  (declare sum-chars (String -> UFix))
  (define (sum-chars s)
    (iter:sum! (map (fn (c) (char:char-code c)) (string:chars s))))

  (declare main-chars (Void -> UFix))
  (define (main-chars)
    (sum-chars "abé"))

  (declare main-integers (Void -> UFix))
  (define (main-integers)
    (+ (string:length (the String (into (the UFix 12345))))
       (+ (* 10 (string:length (the String (into (the I32 -42)))))
          (* 100 (string:length (the String (into (the U8 0))))))))

  (declare greeting (UFix -> String))
  (define (greeting n)
    (if (> n 0)
        (string:concat "hi " (greeting (- n 1)))
        "!"))

  (declare main-built (Void -> UFix))
  (define (main-built)
    (let ((s (greeting 3)))
      (+ (string:length s) (* 100 (string:length (string:concat s s))))))

  (declare main-loop (Void -> UFix))
  (define (main-loop)
    (for ((i 0 (+ i 1))
          (total 0 (+ total (string:length (string:concat "ab" "cde")))))
      :returns total
      :while (< i 1000)
      (values)))

  (declare main-code-char-none (Void -> UFix))
  (define (main-code-char-none)
    (match (char:code-char 1114112)
      ((Some _) 1)
      ((None) 2)))

  (declare main-index-error (Void -> UFix))
  (define (main-index-error)
    (char:char-code (string:ref-unchecked "abc" 5)))

  (declare main-range-error (Void -> UFix))
  (define (main-range-error)
    (string:length (string:substring "hello" 10 20)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (main-char-code)
       (+ (main-char-order)
          (+ (main-code-char)
             (+ (main-ascii)
                (+ (main-length)
                   (+ (main-concat)
                      (+ (main-substring)
                         (+ (main-ref)
                            (+ (main-equal)
                               (+ (main-order)
                                  (+ (main-chars)
                                     (+ (main-integers)
                                        (main-built)))))))))))))))
