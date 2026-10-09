# Status

What runs today, checked by `make test` ([TESTING.md](TESTING.md)).

## Works

| Area | Runs |
|---|---|
| Capture | Compile a Coalton file and keep each binding's typed IR from before optimization |
| Reachability | List the globals a root reaches, with and without IR |
| Monomorphization | Specialize a program at ground types with no dictionaries; the result runs through Coalton's Lisp backend |
| Defunctionalization | Replace function values with tagged unions of the lambdas that reach them; the result runs through Coalton's Lisp backend |
| Lowering | Lower a defunctionalized program to Sheaf's IR, check it and run it on a reference interpreter |
| Prelude | Arrays, slices, references, `Cell`, `Vector` and `Hashtable` that lower, run and get stacks; `fixed:define-length` declares fixed lengths ([PRELUDE.md](PRELUDE.md)) |
| Root checking | Reject a store of an array, slice or reference into storage that outlives what it points into, naming both places ([ROOTS.md](ROOTS.md)) |
| Stack assignment | Assign data stacks to a lowered program and run it on simulated stacks that check each stack's owner |
| C emitter | Emit an assigned program as one C file that runs on `runtime/sheaf.h`; the fixtures' results match the interpreter, including arrays, slices, references, `Cell`, `GrowArray`, `Stack`, `Vector` and `HashTable` |
| SBCL | Every program runs through Coalton's own backend |

## Does not run yet

Only `UFix` and the data types built from it reach C. Each stage's limitations
are in [PIPELINE.md](PIPELINE.md) and the other docs' Limitations sections, and
the [tracker](https://todo.sr.ht/~takeiteasy/sheaf) holds the work.
