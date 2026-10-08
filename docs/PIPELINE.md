# Pipeline

Sheaf uses Coalton as its frontend: Coalton parses and typechecks, and Sheaf
takes over from the typed IR. Every stage but the root checker and the C emitter exists today.

| Stage | Does | Status |
|---|---|---|
| Coalton frontend | Parses, typechecks and translates to codegen IR | Coalton fork |
| Capture | Records each binding's IR before Coalton optimizes it | `ir.lisp` |
| Monomorphization | Specializes every binding at ground types; removes dictionaries ([MONOMORPHIZATION.md](MONOMORPHIZATION.md)) | `monomorphize.lisp` |
| Defunctionalization | Turns function values into tagged unions ([CLOSURES.md](CLOSURES.md)) | `defunctionalize.lisp` |
| Lowering | Turns the result into Sheaf's first-order IR ([IR.md](IR.md)) | `lower.lisp` |
| Data stack assignment | Gives each cell and growable value a stack ([STACKS.md](STACKS.md)) | `stacks.lisp` |
| Root checking | Rejects a reference stored where its root does not outlive it ([MEMORY.md](MEMORY.md)) | planned |
| C emitter | One C file per program | planned |

Coalton's own backend is the development tier: programs compile and run on
SBCL through it, and the REPL works there. Sheaf's checks are meant to run on
that path too, so both accept the same programs.

## Capture

`compile-program` compiles a Coalton source file with the fork's codegen
hook bound, and keeps each binding's IR in a `program`.

```lisp
(let ((program (sheaf:compile-program "main.lisp")))
  (sheaf:reachable program 'my-package::main))
;; => (MAIN MAKE-COUNTER ...)    globals with IR
;;    (COALTON/CLASSES:+ ...)    constructors and class methods
```

| Function | Returns |
|---|---|
| `compile-program` | A `program` holding the IR of every binding the file defines |
| `binding-code` | A global's IR, and whether the program captured it |
| `global-references` | The globals a node refers to |
| `reachable` | Globals reachable from a root, with and without IR |

Captured IR carries a type on every node and a source location on most[^loc].
User code holds raw Lisp only where it writes `(lisp ...)` or uses a macro
that expands to it, such as `assert`.

## Limitations

- Bindings compiled before capture, including all of Coalton's standard
  library, only have Coalton's optimized IR, which inlines raw Lisp:
  [#12](https://todo.sr.ht/~takeiteasy/sheaf/12).
- Some nodes have no location: [#15](https://todo.sr.ht/~takeiteasy/sheaf/15).
- Raw Lisp in user code, such as an `assert`, has no native translation:
  [#16](https://todo.sr.ht/~takeiteasy/sheaf/16).
- The SBCL path does not run Sheaf's checks yet:
  [#17](https://todo.sr.ht/~takeiteasy/sheaf/17).
- Root checking: [#4](https://todo.sr.ht/~takeiteasy/sheaf/4).
- The C emitter, exceptions and tail calls:
  [#6](https://todo.sr.ht/~takeiteasy/sheaf/6),
  [#13](https://todo.sr.ht/~takeiteasy/sheaf/13),
  [#14](https://todo.sr.ht/~takeiteasy/sheaf/14).
- `Integer` and overflow semantics: [#9](https://todo.sr.ht/~takeiteasy/sheaf/9).

[^loc]: Nodes returned by Coalton's `translate-expression` and every
    top-level binding have a location. Nodes a translation helper builds
    directly, such as the variable in a call's operator position, do not.
