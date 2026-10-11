;;;; exceptions.lisp
;;;; throw, catch and protect: every kind of branch, nesting, rethrow, and cleanup on both exits.

(defpackage #:sheaf/fixtures/exceptions
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton/cell)))

(in-package #:sheaf/fixtures/exceptions)

(coalton-toplevel
  (define-exception Failure
    (Bad UFix)
    (Worse UFix UFix)
    (Fatal))

  (define-exception Other
    (Odd UFix))

  (declare check (UFix -> UFix))
  (define (check n)
    (cond ((== n 0) (throw (Bad 5)))
          ((== n 1) (throw (Worse 3 4)))
          ((== n 2) (throw Fatal))
          ((== n 3) (throw (Odd 9)))
          (True n)))

  (declare by-constructor (UFix -> UFix))
  (define (by-constructor n)
    (catch (check n)
      ((Bad x) (+ x 100))
      ((Worse a b) (+ a b))
      ((Fatal) 77)
      ((Odd x) (* x 2))))

  (declare by-type (UFix -> UFix))
  (define (by-type n)
    (catch (check n)
      ((the Failure e)
       (match e
         ((Bad x) x)
         ((Worse a b) (* a b))
         ((Fatal) 1)))
      (_ 1000)))

  (declare nested (UFix -> UFix))
  (define (nested n)
    (catch (catch (check n)
             ((Bad x) (+ x 1)))
      ((Worse a b) (+ a (+ b 10)))
      ((the Other _) 55)
      (_ 9)))

  (declare rethrown (UFix -> UFix))
  (define (rethrown n)
    (catch (catch (check n)
             ((the Failure e) (throw e)))
      ((Bad x) (+ x 1000))
      (_ 3)))

  (declare cleaned (UFix -> UFix))
  (define (cleaned n)
    (let ((flag (cell:new 0)))
      (+ (catch (protect (check n)
                   (cell:write! flag 10))
           (_ 1))
         (cell:read flag))))

  (declare main (Void -> UFix))
  (define (main)
    (+ (by-constructor 0) (+ (by-constructor 1) (+ (by-constructor 2) (+ (by-constructor 3) (+ (by-constructor 4) (+ (by-type 1) (+ (by-type 3) (+ (nested 0) (+ (nested 1) (+ (nested 3) (+ (rethrown 0) (+ (rethrown 2) (+ (cleaned 0) (cleaned 6))))))))))))))))
