;;;; roots.lisp
;;;; References stored where their roots outlive the storage: directly,
;;;; through a call, returned, merged by a match, passed round recursion, and
;;;; held by a tuple.

(defpackage #:sheaf/fixtures/roots
  (:use #:coalton #:coalton-prelude #:sheaf/prelude)
  (:local-nicknames
   (#:arr #:sheaf/prelude/arr)
   (#:ref #:sheaf/prelude/ref)))

(in-package #:sheaf/fixtures/roots)

(coalton-toplevel
  (declare outer-into-inner (Void -> UFix))
  (define (outer-into-inner)
    (let ((outer (arr:new 2 5)))
      (let ((inner (arr:new 1 (arr:ref outer 1))))
        (arr:set! inner 0 (arr:ref outer 0))
        (ref:get (arr:index inner 0)))))

  (declare put ((Array (Ref UFix)) * (Ref UFix) -> Unit))
  (define (put a r)
    (arr:set! a 0 r))

  (declare via-put (Void -> UFix))
  (define (via-put)
    (let ((outer (arr:new 2 7)))
      (let ((inner (arr:new 1 (arr:ref outer 0))))
        (put inner (arr:ref outer 1))
        (ref:get (arr:index inner 0)))))

  (declare second-ref ((Array UFix) -> Ref UFix))
  (define (second-ref a)
    (arr:ref a 1))

  (declare fresh-ref (Void -> Ref UFix))
  (define (fresh-ref)
    (let ((a (arr:new 2 9)))
      (arr:ref a 1)))

  (declare use-fresh (Void -> UFix))
  (define (use-fresh)
    (ref:get (fresh-ref)))

  (declare pick (UFix * (Array UFix) * (Array UFix) -> Ref UFix))
  (define (pick n a b)
    (if (== n 0)
        (arr:ref a 0)
        (arr:ref b 0)))

  (declare walk (UFix * (Ref UFix) -> Ref UFix))
  (define (walk n r)
    (if (== n 0)
        r
        (walk (- n 1) r)))

  (declare ping (UFix * (Ref UFix) * (Ref UFix) -> Ref UFix))
  (define (ping n a b)
    (if (== n 0)
        a
        (pong (- n 1) a b)))

  (declare pong (UFix * (Ref UFix) * (Ref UFix) -> Ref UFix))
  (define (pong n a b)
    (if (== n 0)
        b
        (ping (- n 1) a b)))

  (declare pair-of ((Array UFix) -> Tuple (Ref UFix) UFix))
  (define (pair-of a)
    (Tuple (arr:ref a 0) 3))

  (declare main (Void -> UFix))
  (define (main)
    (let ((a (arr:new 2 4)))
      (let ((b (arr:new 2 6)))
        (+ (+ (+ (outer-into-inner) (via-put)) (use-fresh))
           (+ (+ (ref:get (pick 1 a b)) (ref:get (walk 3 (second-ref a))))
              (+ (ref:get (ping 2 (arr:ref a 0) (arr:ref b 0)))
                 (match (pair-of b)
                   ((Tuple r x) (+ (ref:get r) x))))))))))
