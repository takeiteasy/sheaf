;;;; package.lisp

(defpackage #:sheaf/prelude
  (:use #:coalton #:coalton-prelude)
  (:export
   #:FixedArray #:Array #:GrowArray #:Stack #:Slice #:Ref
   #:fixed-length))

(defpackage #:sheaf/prelude/fixed
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:prelude #:sheaf/prelude))
  (:export #:Extent #:new #:define-length))

(defpackage #:sheaf/prelude/arr
  (:use #:coalton #:coalton-prelude)
  (:shadow #:length)
  (:local-nicknames (#:prelude #:sheaf/prelude))
  (:export #:Indexed #:Viewable #:new #:length #:index #:set! #:ref #:slice))

(defpackage #:sheaf/prelude/grow
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:prelude #:sheaf/prelude))
  (:export #:new #:push!))

(defpackage #:sheaf/prelude/stack
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:prelude #:sheaf/prelude))
  (:export #:new #:push! #:pop!))

(defpackage #:sheaf/prelude/vector
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:vector #:coalton/vector))
  (:export #:vector-fold #:vector-foldr #:vector-find-elem #:vector-sort-by!
           #:vector-with-initial-element))

(defpackage #:sheaf/prelude/ref
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:prelude #:sheaf/prelude))
  (:export #:get #:set!))
