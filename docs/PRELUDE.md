# Prelude

`sheaf/prelude` gives a program arrays, slices and references. Coalton's
`Cell`, `Vector` and `HashTable` are lowered the same way. They live on data stacks
([MEMORY.md](MEMORY.md)), and their operations are primitives of Sheaf's IR, so they lower, run and get stacks assigned like any other code.

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
| `(Vector :t)` | Resizable, grow-shrink | Elements; Coalton's `coalton/vector:Vector`, with no references or slices |
| `(HashTable :k :v)` | Resizable, grow-shrink | Entries; Coalton's `coalton/hashtable:HashTable`, with no references or slices |
| `String` | Variable | UTF-8 bytes; Coalton's `String` ([STRINGS.md](STRINGS.md)) |

A `HashTable` has `UFix` keys and any value type. Its `Hash` and `Eq`
instances are not consulted: keys compare as numbers.

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
| `coalton/vector` | `(new)`, `(push! x v)`, `(pop! v)` | A `Vector`, an append that returns the new index, and a removal that returns an `Optional` |
| `coalton/vector` | `(index i v)`, `(index-unsafe i v)`, `(set! i x v)`, `(length v)` | An `Optional` element, an element, a write, and the length |
| `coalton/vector` | `(copy v)`, `(subseq v start end)`, `(resect! v start end)`, `(reverse! v)`, `(pop-unsafe! v)` | A new `Vector`, a new `Vector` of a range, a removal of a range, an in-place reversal, and a removal of the last element that is an error when empty; `start` and `end` may be in either order and past the end |
| `coalton/vector` | `(with-capacity n)`, `(capacity v)`, `(set-capacity! n v)` | A `Vector`, its length, and a truncation to `n` when `n` is below the length[^capacity] |
| `coalton/vector` | `fold`, `foldr`, `find-elem`, `sort-by!`, `sort!` | A fold from either end, the index of an element, and an in-place stable sort[^replaced] |
| `coalton/hashtable` | `entries`, `keys`, `values-iter`, `extend!` | Iterators over a table, in no fixed order, and an insertion of every pair of an iterator |
| `coalton/list` | `%reverse!` | A list reversed in place, which `map`, `take` and `split` use on a list they built[^reverse] |
| `coalton/hashtable` | `(new)`, `(with-capacity n)`, `(make (k v) ...)` | A `HashTable`; the capacity is a hint |
| `coalton/hashtable` | `(get h k)`, `(set! h k x)`, `(remove! h k)`, `(count h)` | An `Optional` value, a write, a removal, and the number of entries |
| `coalton/cell` | `(new x)`, `(read c)`, `(write! c x)`, `(swap! c x)` | A `Cell`, its contents, and a write that returns the new or the old value |

Mutating operations return `Unit`. An index outside an array is an error.
Arrays are shared, not copied, when bound to another name.

## Replacements

A standard library function whose body is raw Lisp can be replaced by a
function of `sheaf/prelude` written in Coalton. Monomorphization takes the
replacement's code where it would take the original's, so the program runs the
replacement on every tier except Coalton's own backend for the standard library,
which keeps the original.

| Replaced | By |
|---|---|
| `Foldable Vector` `fold` and `foldr` | `vector-fold`, `vector-foldr` |
| `coalton/vector:find-elem` | `vector-find-elem` |
| `coalton/vector:with-initial-element` | `vector-with-initial-element`, since the original's iterator holds an assertion |
| `coalton/hashtable:entries`, `keys`, `values-iter` | `hashtable-entries`, `hashtable-keys`, `hashtable-values-iter`, a cursor over the table's slots[^cursor] |
| `coalton/vector:sort-by!` | `vector-sort-by!`, an insertion sort[^sort] |
| `coalton/char:code-char`, `ascii-alpha?`, `ascii-digit?`, `ascii-uppercase?`, `ascii-lowercase?`, `digit->ufix` | `char-code-char`, `char-ascii-alpha?` and the rest, over `char-code` |
| `Semigroup String` `<>` | `string-append`, a call of `concat` |

## Where an array is built

An array is built like any other storage ([STACKS.md](STACKS.md)): in the
caller's destination when it may reach the result, otherwise in the
function's pool. A `GrowArray`, `Stack`, `Vector` or `HashTable` has a stack of
its own: its caller gives the function one for each it returns, and a `let`
that binds one owns one. The interpreter checks the value is on top of its
stack when it grows.

## Limitations

- A `HashTable` key other than `UFix` is a lowering error:
  [#20](https://github.com/takeiteasy/sheaf/issues/20).
- Other standard library functions that hold raw Lisp, such as `String` `split`
  and `Char` `upcase`, are errors:
  [#50](https://github.com/takeiteasy/sheaf/issues/50).

[^capacity]: Capacity is a hint. A `Vector` has no capacity of its own, so `capacity`
    is its length, and Coalton's backend reports the room it holds, which is
    never less. A program that reads the number sees different values.

[^reverse]: The cells of the list are relinked, so it is only safe on a list
    whose cells outlive none of the cells they point to after the relink: the
    standard library uses it on lists it has just built.

[^replaced]: The replacements are in `prelude/vector.lisp`. Each has the type and
    constraints of the function it stands for, so the program's call needs no
    change. The code's loops and returns lower like any other.

[^sort]: Stable, where Coalton's backend sorts with `cl:sort`, which is not, so
    elements that compare equal may end in a different order on Coalton's backend.
    A sort of a long vector is slow: [#45](https://github.com/takeiteasy/sheaf/issues/45).

[^cursor]: The iterators call `table-next-slot`, `table-slot-key` and
    `table-slot-value`, which are primitives. In C a slot is a position in the
    table's slots; on Coalton's backend, the position in its order, which a lookup
    walks to, so an iteration there takes time quadratic in the table's size.

[^len]: A `FixedArray` length is read from the type after monomorphization,
    through a table `define-length` fills. The `Extent` class that
    `define-length` instantiates gives `fixed:new` the same length on
    Coalton's own backend. The operations are matched by the origin of the
    function or instance method, so their bodies are never lowered.
