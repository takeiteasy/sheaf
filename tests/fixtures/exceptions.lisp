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

  (declare cleanup-catches (UFix -> UFix))
  (define (cleanup-catches n)
    (catch (protect (check n)
             (catch (the UFix (throw (Odd 1)))
               ((Bad z) z)
               ((Odd y) y)))
      ((Bad x) (+ x 40))
      (_ 3)))

  (declare cleanup-throws (UFix -> UFix))
  (define (cleanup-throws n)
    (catch (protect (check n)
             (the UFix (throw Fatal)))
      ((Bad x) x)
      ((Fatal) 77)))

  (declare nested-cleanups (UFix -> UFix))
  (define (nested-cleanups n)
    (let ((a (cell:new 0))
          (b (cell:new 0)))
      (+ (catch (protect (protect (check n)
                           (cell:write! a 3))
                   (cell:write! b 20))
           (_ 100))
         (+ (cell:read a) (cell:read b)))))

  (declare main-cleanup-catches (Void -> UFix))
  (define (main-cleanup-catches)
    (+ (cleanup-catches 0) (cleanup-catches 4)))

  (declare main-cleanup-throws (Void -> UFix))
  (define (main-cleanup-throws)
    (+ (cleanup-throws 0) (cleanup-throws 4)))

  (declare main-nested-cleanups (Void -> UFix))
  (define (main-nested-cleanups)
    (+ (nested-cleanups 0) (nested-cleanups 4)))

  (declare main (Void -> UFix))
  (define (main)
    (+ (by-constructor 0) (+ (by-constructor 1) (+ (by-constructor 2) (+ (by-constructor 3) (+ (by-constructor 4) (+ (by-type 1) (+ (by-type 3) (+ (nested 0) (+ (nested 1) (+ (nested 3) (+ (rethrown 0) (+ (rethrown 2) (+ (cleaned 0) (+ (cleaned 6) (+ (main-cleanup-catches) (+ (main-cleanup-throws) (main-nested-cleanups)))))))))))))))))))
