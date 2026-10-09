;;;; roots.lisp
;;;; References stored where their roots outlive the storage: directly,
;;;; through a call, returned, merged by a match, passed round recursion, held
;;;; by a tuple, and leaving a branch or a function that holds them.

(defpackage #:sheaf/fixtures/roots
  (:use #:coalton #:coalton-prelude #:sheaf/prelude)
  (:local-nicknames
   (#:arr #:sheaf/prelude/arr)
   (#:grow #:sheaf/prelude/grow)
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

  (declare escape-branch (Void -> UFix))
  (define (escape-branch)
    (let ((base (arr:new 1 3)))
      (let ((r (if (== (the UFix 1) 1)
                   (let ((a (arr:new 1 4)))
                     (arr:ref a 0))
                   (arr:ref base 0))))
        (ref:get r))))

  (declare return-holding (Void -> Array (Ref UFix)))
  (define (return-holding)
    (let ((inner (arr:new 1 5)))
      (arr:new 1 (arr:ref inner 0))))

  (declare return-stored (Void -> Array (Ref UFix)))
  (define (return-stored)
    (let ((inner (arr:new 1 6)))
      (let ((out (arr:new 1 (arr:ref inner 0))))
        (arr:set! out 0 (arr:ref inner 0))
        out)))

  (declare use-returned (Void -> UFix))
  (define (use-returned)
    (+ (ref:get (arr:index (return-holding) 0))
       (ref:get (arr:index (return-stored) 0))))

  (declare grown-ref (UFix -> Ref UFix))
  (define (grown-ref n)
    (let ((r (if (== n 0)
                 (let ((g (the (GrowArray UFix) (grow:new))))
                   (grow:push! g 7)
                   (arr:ref g 0))
                 (let ((h (the (GrowArray UFix) (grow:new))))
                   (grow:push! h 9)
                   (arr:ref h 0)))))
      r))

  (declare use-grown (Void -> UFix))
  (define (use-grown)
    (+ (ref:get (grown-ref 0)) (ref:get (grown-ref 1))))

  (declare grown-branch (UFix -> UFix))
  (define (grown-branch n)
    (let ((v (if (== n 0)
                 (let ((g (the (GrowArray UFix) (grow:new))))
                   (grow:push! g 11)
                   g)
                 (let ((h (the (GrowArray UFix) (grow:new))))
                   (grow:push! h 13)
                   h))))
      (arr:index v 0)))

  (declare escapes (Void -> UFix))
  (define (escapes)
    (+ (+ (escape-branch) (use-returned))
       (+ (use-grown) (grown-branch 0))))

  (declare main (Void -> UFix))
  (define (main)
    (let ((a (arr:new 2 4)))
      (let ((b (arr:new 2 6)))
        (+ (+ (+ (outer-into-inner) (via-put)) (+ (use-fresh) (escapes)))
           (+ (+ (ref:get (pick 1 a b)) (ref:get (walk 3 (second-ref a))))
              (+ (ref:get (ping 2 (arr:ref a 0) (arr:ref b 0)))
                 (match (pair-of b)
                   ((Tuple r x) (+ (ref:get r) x))))))))))
