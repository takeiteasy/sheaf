;;;; cells.lisp
;;;; Cells built locally, returned, swapped, and captured by a closure.

(defpackage #:sheaf/fixtures/cells
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton/cell)))

(in-package #:sheaf/fixtures/cells)

(coalton-toplevel
  (declare local-sum (UFix -> UFix))
  (define (local-sum n)
    (let ((c (cell:new n)))
      (cell:write! c (+ (cell:read c) 1))
      (cell:read c)))

  (declare fresh-cell (UFix -> Cell UFix))
  (define (fresh-cell n)
    (cell:new n))

  (declare bump (UFix -> UFix))
  (define (bump n)
    (let ((c (fresh-cell n)))
      (cell:write! c (* (cell:read c) 2))))

  (declare swapped (UFix -> UFix))
  (define (swapped n)
    (let ((c (cell:new n)))
      (+ (cell:swap! c 10) (cell:read c))))

  (declare make-counter (Void -> (Void -> UFix)))
  (define (make-counter)
    (let ((c (cell:new 0)))
      (fn () (cell:write! c (+ (cell:read c) 1)))))

  (declare main (Void -> UFix))
  (define (main)
    (let ((next (make-counter)))
      (next)
      (+ (local-sum 3)
         (+ (bump 4)
            (+ (swapped 5) (next)))))))
