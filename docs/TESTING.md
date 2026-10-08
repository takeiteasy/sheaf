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
| `escape.lisp` | A function passed to raw Lisp, which defunctionalization rejects |

`make test` loads `vendor/coalton` ahead of any Coalton Quicklisp has. It
reads `QUICKLISP` (default `~/quicklisp/setup.lisp`) and `SBCL`.
