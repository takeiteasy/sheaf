# Pipeline

Sheaf uses Coalton as its frontend: Coalton parses and typechecks, and Sheaf
takes over from the typed IR.

| Stage | Does | Status |
|---|---|---|
| Coalton frontend | Parses, typechecks and translates to codegen IR | Coalton fork |
| Capture | Records each binding's IR before Coalton optimizes it | `ir.lisp` |
| Monomorphization | Specializes every binding at ground types; removes dictionaries ([MONOMORPHIZATION.md](MONOMORPHIZATION.md)) | `monomorphize.lisp` |
| Defunctionalization | Turns function values into tagged unions ([CLOSURES.md](CLOSURES.md)) | `defunctionalize.lisp` |
| Lowering | Turns the result into Sheaf's first-order IR ([IR.md](IR.md)) | `lower.lisp` |
| Root checking | Rejects a reference stored where what it points into is freed first ([ROOTS.md](ROOTS.md)) | `roots.lisp` |
| Data stack assignment | Gives each cell and growable value a stack ([STACKS.md](STACKS.md)) | `stacks.lisp` |
| C emitter | One C file per program ([EMITTER.md](EMITTER.md)) | `emit.lisp` |

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
| `binding-code` | A global's IR, and whether the program captured it; every binding has IR but constructors, class methods and dictionary constructors |
| `global-references` | The globals a node refers to |
| `reachable` | Globals reachable from a root, with and without IR |

Captured IR carries a type and a source location on every node[^loc].
User code holds raw Lisp only where it writes `(lisp ...)` or uses a macro
that expands to it, such as `assert`.

A binding compiled before capture, such as the standard library, has the IR
Coalton kept from before it optimized, which `binding-code` returns with
`nil` for captured[^source]. That code holds raw Lisp only where the source
does.

## Limitations

- Standard library functions whose source holds raw Lisp stay raw Lisp:
  [#36](https://github.com/takeiteasy/sheaf/issues/36).
- Raw Lisp in user code, such as an `assert`, has no native translation:
  [#7](https://github.com/takeiteasy/sheaf/issues/7).
- The SBCL path does not run Sheaf's checks yet:
  [#8](https://github.com/takeiteasy/sheaf/issues/8).
- `handle`, `resumable` and `resume-to` are errors: [#24](https://github.com/takeiteasy/sheaf/issues/24).
- `Integer` is 64 bits: [#19](https://github.com/takeiteasy/sheaf/issues/19).

[^loc]: The fork stamps the node each translation method returns. A node a
    helper builds takes the location of the expression being translated,
    except a variable built from a source variable, which takes that
    variable's own location.

[^source]: The fork keeps each binding's IR in the environment as it is
    translated, so it reaches the compiled files of every library. It adds
    about 5% to compiled size and 3 MB to a loaded image.
