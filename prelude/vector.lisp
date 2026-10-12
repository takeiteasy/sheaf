;;;; vector.lisp
;;;; Vector functions whose standard library code is raw Lisp, written in Coalton.
;;;; Lowering uses these in place of the originals.

(in-package #:sheaf/prelude/vector)

(coalton-toplevel
  (declare vector-fold ((:accum * :elt -> :accum) * :accum * (vector:Vector :elt) -> :accum))
  (define (vector-fold f init v)
    (for ((i 0 (+ i 1))
          (accum init (f accum (vector:index-unsafe i v))))
      :returns accum
      :while (< i (vector:length v))
      (values)))

  (declare vector-foldr ((:elt * :accum -> :accum) * :accum * (vector:Vector :elt) -> :accum))
  (define (vector-foldr f init v)
    (for ((i (vector:length v) (- i 1))
          (accum init (f (vector:index-unsafe (- i 1) v) accum)))
      :returns accum
      :while (> i 0)
      (values)))

  (declare vector-with-initial-element (UFix * :elt -> (vector:Vector :elt)))
  (define (vector-with-initial-element n x)
    (let ((v (vector:with-capacity n)))
      (for ((i 0 (+ i 1)))
        :while (< i n)
        (vector:push! x v)
        (values))
      v))

  (declare vector-find-elem (Eq :elt => :elt * (vector:Vector :elt) -> Optional UFix))
  (define (vector-find-elem e v)
    (for ((i 0 (+ i 1)))
      :returns None
      :while (< i (vector:length v))
      (when (== (vector:index-unsafe i v) e)
        (return (Some i)))
      (values)))

  ;; TODO: insertion sort, O(n^2); a merge or heap sort if long vectors are sorted
  ;; https://github.com/takeiteasy/sheaf/issues/45
  (declare vector-sort-by! ((:elt * :elt -> Boolean) * (vector:Vector :elt) -> Void))
  (define (vector-sort-by! before? v)
    (for ((i 1 (+ i 1)))
      :while (< i (vector:length v))
      (let ((x (vector:index-unsafe i v)))
        (let ((place (for ((j i (- j 1)))
                       :returns j
                       :while (and (> j 0)
                                   (before? x (vector:index-unsafe (- j 1) v)))
                       (vector:set! j (vector:index-unsafe (- j 1) v) v)
                       (values))))
          (vector:set! place x v)
          (values))))
    (values)))
