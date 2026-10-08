# Roots

`check-roots` rejects a program that stores a reference where its root does
not outlive the storage. It reads the lowered IR ([IR.md](IR.md)) and needs no
stack assignment ([STACKS.md](STACKS.md)).

```lisp
(let ((ir (sheaf:lower (sheaf:defunctionalize (sheaf:monomorphize program 'my-package::main)))))
  (sheaf:check-roots ir))   ; => a table of summaries, or an error
```

```lisp
(let ((outer (arr:new 1 (arr:ref base 0))))      ; an array of references
  (let ((inner (arr:new 1 0)))
    (arr:set! outer 0 (arr:ref inner 0))))       ; error: inner does not outlive outer
```

```text
Root checking rejects storing a reference rooted at INNER-72 into storage rooted at OUTER-71, which outlives it
  ARR-SET-INNER at a value of type REF UFIX, offset 1216
  ARR-SET-INNER at the store elements-set, offset 1198
```

The error names the reference and the store, each with its source location.
A store inside a callee also names the call.

## What has a root

| Value | Root |
|---|---|
| An array | The `let` that builds it |
| A `Ref` or `Slice` | The roots of the array it points into |
| A parameter | The caller's argument |
| A global | A global |
| A tuple or other data | Every root of the references it holds |
| A value read out of an array | The roots of the array[^inexact] |

Only values that are, or hold, an array, slice or reference have roots. A
list of `UFix` has none. `Cell`, `Vector` and `Hashtable` have none either
([#8](https://todo.sr.ht/~takeiteasy/sheaf/8)).

## The rule

A store of a reference into an array or reference is accepted when the root of
the reference outlives the root of the destination: a root made outside a
`let` outlives one made inside it, and parameters and globals outlive every
local.

| Stored | Into | Result |
|---|---|---|
| A global | Anything | Accepted |
| A parameter | A local | Accepted |
| A parameter | The same parameter | Accepted |
| A parameter | Another parameter, or a global | Left for the caller to check |
| A local | A local made no later than it | Accepted |
| A local | A parameter, a global, or an older local | Error |

A value that leaves a `match` branch may not be rooted in a local the branch
makes.

## Summaries

Each function has a `root-summary`, which the callers of that function use to
check the stores it leaves for them.

| Slot | Holds |
|---|---|
| `root-summary-result` | What the result may point into: parameter numbers, `:global`, `:fresh` for storage built in the caller's destination |
| `root-summary-stores` | Each store the function makes of one parameter into another or into a global |
| `root-summary-locals` | The roots of every name in the function |

```lisp
(define (put a r) (arr:set! a 0 r))   ; stores parameter 1 into parameter 0
```

A recursive cycle repeats until its summaries stop growing, then reports
errors.

## Returning storage

A function may return a reference into an array it builds: the array is built
in the caller's destination ([STACKS.md](STACKS.md)). A returned array may not
hold a reference into another local.

## Limitations

- `Cell`, `Vector` and `Hashtable` are not checked:
  [#8](https://todo.sr.ht/~takeiteasy/sheaf/8).
- Stack assignment does not use roots, and does not follow stores into arrays:
  [#25](https://todo.sr.ht/~takeiteasy/sheaf/25).
- The SBCL path does not run the check:
  [#17](https://todo.sr.ht/~takeiteasy/sheaf/17).

[^inexact]: A value read out of an array gets the roots of the array, which
    bound its lifetime without naming where it points. A reference read out of
    an array and stored into a younger array is accepted only if the array it
    was read from outlives that array too.
