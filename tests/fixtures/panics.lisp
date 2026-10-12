;;;; panics.lisp
;;;; assert, error and unreachable, caught by `_` and around a protect.

(defpackage #:sheaf/fixtures/panics
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton/cell)))

(in-package #:sheaf/fixtures/panics)

(coalton-toplevel
  (declare checked (UFix -> UFix))
  (define (checked n)
    (assert (> n 0))
    (if (> n 50) (error "too big") (+ n 1)))

  (declare caught (UFix -> UFix))
  (define (caught n)
    (catch (checked n)
      (_ 1000)))

  (declare cleaned (UFix -> UFix))
  (define (cleaned n)
    (let ((flag (cell:new 0)))
      (+ (catch (protect (checked n)
                   (cell:write! flag 10))
           (_ 1))
         (cell:read flag))))

  (declare unreached (Boolean -> UFix))
  (define (unreached b)
    (catch (if b 1 (unreachable))
      (_ 7)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (caught 3) (+ (caught 0) (+ (caught 60) (+ (cleaned 0) (+ (cleaned 4) (+ (unreached True) (unreached False)))))))))
