# Testing

```sh
make test
```

The tests use [FiveAM](https://github.com/lispci/fiveam) and live in
`tests/`. A test compiles a Coalton program from `tests/fixtures/` with
`compile-program` and checks the captured IR.

| Fixture | Exercises |
|---|---|
| `counter.lisp` | An iterator chain, and a closure returned over a `Cell` |

`make test` loads `vendor/coalton` ahead of any Coalton Quicklisp has. It
reads `QUICKLISP` (default `~/quicklisp/setup.lisp`) and `SBCL`.
