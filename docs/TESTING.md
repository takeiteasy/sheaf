# Testing

```sh
make test
```

The tests use [FiveAM](https://github.com/lispci/fiveam) and live in
`tests/`. A test compiles a Coalton program from `tests/fixtures/` with
`compile-program`, then checks the captured IR or its monomorphized form.
Monomorphized programs are also compiled back to Lisp and run.

| Fixture | Exercises |
|---|---|
| `counter.lisp` | An iterator chain, and a closure returned over a `Cell` |
| `poly.lisp` | Constrained generics at two types, a constrained instance, superclass access, a generic local |
| `polyrec.lisp` | Polymorphic recursion, which monomorphization rejects |

`make test` loads `vendor/coalton` ahead of any Coalton Quicklisp has. It
reads `QUICKLISP` (default `~/quicklisp/setup.lisp`) and `SBCL`.
