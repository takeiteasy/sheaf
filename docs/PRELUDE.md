# Prelude

`sheaf/prelude` gives a program arrays, slices and references that live on
data stacks ([MEMORY.md](MEMORY.md)). Their operations are primitives of
Sheaf's IR, so they lower, run and get stacks assigned like any other code.

```lisp
(defpackage #:my-program
  (:use #:coalton #:coalton-prelude #:sheaf/prelude)
  (:local-nicknames (#:fixed #:sheaf/prelude/fixed) (#:arr #:sheaf/prelude/arr)
                    (#:grow #:sheaf/prelude/grow) (#:ref #:sheaf/prelude/ref)))

(fixed:define-length N4 4)

(coalton-toplevel
  (define (sum-two)
    (let ((a (the (FixedArray N4 UFix) (fixed:new 3))))
      (let ((r (arr:ref a 1)))
        (ref:set! r 10)
        (+ (ref:get r) (arr:index a 0))))))   ; => 13
```

## Types

| Type | Size class | Holds |
|---|---|---|
| `(FixedArray :n :t)` | Fixed | `:n` elements, a length declared with `define-length` |
| `(Array :t)` | Variable | A length chosen when it is built |
| `(GrowArray :t)` | Resizable, grow-only | Elements; references into it stay valid |
| `(Stack :t)` | Resizable, grow-shrink | Elements; it has no references or slices |
| `(Slice :t)` | View | A range of an array |
| `(Ref :t)` | View | One element of an array |
| `(Cell :t)` | Fixed | One element; Coalton's `coalton/cell:Cell` |

`Vector` and `Hashtable` are not part of the prelude
([#29](https://todo.sr.ht/~takeiteasy/sheaf/29),
[#30](https://todo.sr.ht/~takeiteasy/sheaf/30)).

## Operations

| Package | Function | Does |
|---|---|---|
| `fixed` | `(define-length N 4)` | Declares the length type `N`; used outside `coalton-toplevel`[^len] |
| `fixed` | `(new x)` | A `FixedArray` of `x`; the length comes from its type |
| `arr` | `(new n x)` | An `Array` of `n` copies of `x` |
| `arr` | `(length a)`, `(index a i)`, `(set! a i x)` | On every array and `Slice` |
| `arr` | `(ref a i)`, `(slice a lo hi)` | A `Ref` or `Slice` into everything but a `Stack` |
| `grow` | `(new)`, `(push! a x)` | A `GrowArray`, and an append |
| `stack` | `(new)`, `(push! s x)`, `(pop! s)` | A `Stack`; `pop!` returns an `Optional` |
| `ref` | `(get r)`, `(set! r x)` | Read and write the element |
| `coalton/cell` | `(new x)`, `(read c)`, `(write! c x)`, `(swap! c x)` | A `Cell`, its contents, and a write that returns the new or the old value |

Mutating operations return `Unit`. An index outside an array is an error.
Arrays are shared, not copied, when bound to another name.

## Where an array is built

An array is built like any other storage ([STACKS.md](STACKS.md)): in the
caller's destination when it may reach the result, otherwise in the
function's pool. A `GrowArray` or `Stack` bound by a `let` owns a stack for
that `let`, and the interpreter checks it is on top of the stack when it
grows.

## Limitations

- The C emitter rejects the prelude's types and operations, and `Cell`:
  [#27](https://todo.sr.ht/~takeiteasy/sheaf/27).
- Elements other than `UFix` and the data types built from it have no
  primitives: [#23](https://todo.sr.ht/~takeiteasy/sheaf/23).
- A returned `GrowArray` built in a destination is not guaranteed to stay on
  top of its stack: [#24](https://todo.sr.ht/~takeiteasy/sheaf/24).

[^len]: A `FixedArray` length is read from the type after monomorphization,
    through a table `define-length` fills. The `Extent` class that
    `define-length` instantiates gives `fixed:new` the same length on
    Coalton's own backend. The operations are matched by the origin of the
    function or instance method, so their bodies are never lowered.
