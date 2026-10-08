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
| Stack assignment | Assign data stacks to a lowered program and run it on simulated stacks that check each stack's owner |
| C emitter | Emit an assigned program as one C file that runs on `runtime/sheaf.h`; the fixtures' results match the interpreter |
| SBCL | Every program runs through Coalton's own backend |

## Does not run yet

Root checking. [PIPELINE.md](PIPELINE.md) lists each stage, and the
[tracker](https://todo.sr.ht/~takeiteasy/sheaf) holds the work.
