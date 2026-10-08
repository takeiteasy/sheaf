# Status

What runs today, checked by `make test` ([TESTING.md](TESTING.md)).

## Works

| Area | Runs |
|---|---|
| Capture | Compile a Coalton file and keep each binding's typed IR from before optimization |
| Reachability | List the globals a root reaches, with and without IR |
| Monomorphization | Specialize a program at ground types with no dictionaries; the result runs through Coalton's Lisp backend |
| SBCL | Every program runs through Coalton's own backend |

## Does not run yet

Defunctionalization, root checking, data stack assignment
and the C emitter. [PIPELINE.md](PIPELINE.md) lists each stage, and the
[tracker](https://todo.sr.ht/~takeiteasy/sheaf) holds the work.
