# Testing

```sh
make test
```

The tests use [FiveAM](https://github.com/lispci/fiveam) and live in
`tests/`. A test compiles a Coalton program from `tests/fixtures/` with
`compile-program`, then checks the captured IR, its monomorphized form, its
defunctionalized form, its lowered form or its stack assignment. Monomorphized and defunctionalized
programs are also compiled back to Lisp and run; lowered ones run on
`run-lowered`.

| Fixture | Exercises |
|---|---|
| `counter.lisp` | An iterator chain, and a closure returned over a `Cell` |
| `poly.lisp` | Constrained generics at two types, a constrained instance, superclass access, a generic local |
| `polyrec.lisp` | Polymorphic recursion, which monomorphization rejects |
| `closures.lisp` | Lambdas reaching one slot, a function in a constructor, a returned closure, a recursive local function, a global used as a value |
| `flows.lisp` | Functions as type arguments, in a list and bound to a global value; a recursive local function that captures a variable and passes itself on |
| `nested.lisp` | A closure nested to a depth known only at runtime |
| `recursive-data.lisp` | A data type holding a function, and a list of them |
| `nested-optional.lisp` | A closure nested through an `Optional` to a runtime depth |
| `stacks.lisp` | Lists built and returned, built and dropped, built in a recursive cycle, and held by a global |
| `tail.lisp` | A self tail call run ten million times, and a list built by one |
| `mutual-tail.lisp` | Tail calls between functions, millions deep: a pair, and a cycle of three with different parameters |
| `arrays.lisp` | Every prelude type and operation, run on SBCL, the interpreter and assigned stacks |
| `bad-length.lisp` | A length type `define-length` does not declare |
| `out-of-bounds.lisp` | An index past the end of an array |
| `roots.lisp` | References stored outward and between storage freed together, through a call, returned, merged by a match, passed round recursion and held by a tuple |
| `roots-rejected.lisp` | Stores of a reference into storage that outlives what it points into: directly, through an array that holds it, through calls, into a parameter, into a global, out of a branch that frees what it points into, and through recursion |
| `cells.lisp` | Cells built locally, returned, swapped and captured by a closure |
| `growables.lisp` | Results that hold growable values, each on a stack of its own, and cycles that build them at every level |
| `growable-recursion.lisp` | A growable local kept across a call back into its recursive cycle, which stack assignment rejects |
| `hashtables.lisp` | Hash tables built locally and returned, and every operation on them |
| `numbers.lisp` | Arithmetic, equality and ordering of each numeric type, overflow, and arrays and hash tables of numbers, on SBCL, the interpreter and in C |
| `own-indexed.lisp` | A program's own instance of the prelude's `Indexed` class, which stays an ordinary function |
| `placement.lisp` | Where storage is built when a result holds only some of it |
| `vectors.lisp` | Vectors built locally and returned, and every operation on them |
| `escape.lisp` | A function passed to raw Lisp, which defunctionalization rejects |

`tests/c-ast.lisp` checks the C printer's parentheses and layout.
`tests/emit.lisp` compiles the emitted C with `$CC` (default `cc`), so `make test` needs a C compiler.

## CI

`.github/workflows/test.yml` runs `make test` on Ubuntu with `CC=gcc` and
`CC=clang`, on pushes and pull requests to `trunk`. Docs-only changes skip it,
and a newer push cancels the run it supersedes. SBCL 2.6.8 comes from a pinned
binary release. `workflow_dispatch` runs it by hand.

`make test` loads `vendor/coalton` ahead of any Coalton Quicklisp has. It
reads `QUICKLISP` (default `~/quicklisp/setup.lisp`) and `SBCL`.
