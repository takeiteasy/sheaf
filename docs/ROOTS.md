# Roots

`check-roots` rejects a program that stores a reference where its root does
not outlive the storage, or that lets a reference leave a branch that frees
what it points into. It reads the lowered IR ([IR.md](IR.md)) and works out
which growable stacks stack assignment frees without assigning any; stack
assignment ([STACKS.md](STACKS.md)) uses its summaries.

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
Root checking rejects storing a reference rooted at INNER-17 into storage rooted at OUTER-16, which outlives it
  ARR-SET-INNER<Void → UFIX>{0} at a value of type REF INTEGER, offset 1216
  ARR-SET-INNER<Void → UFIX>{0} at the store elements-set, offset 1198
```

The error names the reference and the store, each with its source location.
A store inside a callee also names the call.

## What has a root

| Value | Root |
|---|---|
| An array or `Cell` | The `let` that builds it |
| A `Ref` or `Slice` | The roots of the array it points into |
| A parameter | The caller's argument |
| A global | A global |
| A list or other self-containing data | The `let` that builds it, which holds the roots of its fields |
| A tuple or other data | Every root of the references it holds |
| A value read out of an array | The roots of the array[^inexact] |

Only values that are, or hold, storage have roots: an array, slice, reference,
`Cell`, `Vector`, `Hashtable` or list. A `UFix` has none.

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

A reference may not leave a `match` branch when it points into a growable
value the branch frees[^freed]. Arrays, `Cell`s and lists made in a branch live
until the function returns, so a reference to them may leave.

```lisp
(let ((r (if (== n 0)
             (let ((g (grow:new)))
               (grow:push! g 7)
               (arr:ref g 0))                  ; error: g is freed when the branch ends
             (arr:ref base 0))))
  (ref:get r))
```

## Summaries

Each function has a `root-summary`, which the callers of that function use to
check the stores it leaves for them.

| Slot | Holds |
|---|---|
| `root-summary-result` | What the result may point into: parameter numbers, `:global`, `:fresh` for storage built in the caller's destinations |
| `root-summary-stores` | Each store the function makes of one parameter into another or into a global |
| `root-summary-locals` | The roots of every name in the function |
| `root-summary-reached` | The locals whose storage the result holds, which stack assignment builds in the caller's destinations |

```lisp
(define (put a r) (arr:set! a 0 r))   ; stores parameter 1 into parameter 0
```

A recursive cycle repeats until its summaries stop growing, then reports
errors.

## Returning storage

A function may return a reference into an array it builds, and the array may
hold references into its other locals: whatever the result reaches is built in
the caller's destinations ([STACKS.md](STACKS.md)).

## Limitations

- The store rule compares `let` depths and rejects some safe stores between
  storage in the same pool: [#39](https://todo.sr.ht/~takeiteasy/sheaf/39).
- The SBCL path does not run the check:
  [#17](https://todo.sr.ht/~takeiteasy/sheaf/17).

[^inexact]: A value read out of an array gets the roots of the array, which
    bound its lifetime without naming where it points. A reference read out of
    an array and stored into a younger array is accepted only if the array it
    was read from outlives that array too.

[^freed]: A growable local is freed when its `let` ends, so a branch frees the
    ones it binds unless the result holds them, or the branch returns them
    to a `let` outside it. Roots are one per `let`, so a call result that holds
    both a cell and a growable value has one root: a reference into its cell
    part leaving a branch is rejected when the growable part is freed.
