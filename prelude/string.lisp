;;;; string.lisp
;;;; Char functions whose standard library code is raw Lisp, written in Coalton.
;;;; Lowering uses these in place of the originals.

(in-package #:sheaf/prelude/string)

(coalton-toplevel
  (declare string-append (String * String -> String))
  (define (string-append a b)
    (string:concat a b))

  (declare char-code-char (UFix -> Optional Char))
  (define (char-code-char code)
    (if (< code 1114112)
        (Some (char::code-char-unchecked code))
        None))

  (declare char-ascii-alpha? (Char -> Boolean))
  (define (char-ascii-alpha? c)
    (let ((code (char:char-code c)))
      (or (and (<= 65 code) (<= code 90))
          (and (<= 97 code) (<= code 122)))))

  (declare char-ascii-digit? (Char -> Boolean))
  (define (char-ascii-digit? c)
    (let ((code (char:char-code c)))
      (and (<= 48 code) (<= code 57))))

  (declare char-ascii-uppercase? (Char -> Boolean))
  (define (char-ascii-uppercase? c)
    (let ((code (char:char-code c)))
      (and (<= 65 code) (<= code 90))))

  (declare char-ascii-lowercase? (Char -> Boolean))
  (define (char-ascii-lowercase? c)
    (let ((code (char:char-code c)))
      (and (<= 97 code) (<= code 122))))

  (declare char-digit->ufix (Char -> Optional UFix))
  (define (char-digit->ufix c)
    (let ((code (char:char-code c)))
      (if (and (<= 48 code) (<= code 57))
          (Some (- code 48))
          None))))
