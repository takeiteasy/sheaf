# Roots

`check-roots` rejects a program that stores a reference into storage that
outlives what the reference points into, or that lets a reference leave a
branch that frees what it points into. It reads the lowered IR ([IR.md](IR.md))
and works out which growable stacks stack assignment frees without assigning
any; stack assignment ([STACKS.md](STACKS.md)) uses its summaries.

```lisp
(let ((ir (sheaf:lower (sheaf:defunctionalize (sheaf:monomorphize program 'my-package::main)))))
  (sheaf:check-roots ir))   ; => a table of summaries, or an error
```

```lisp
(let ((outer (arr:new 1 (arr:ref base 0))))      ; an array of references
  (if (== n 0)
      (let ((g (grow:new)))
        (grow:push! g 7)
        (arr:set! outer 0 (arr:ref g 0)))        ; error: g is freed before outer
      0))
```

```text
Root checking rejects storing a reference rooted at G-17 into storage rooted at OUTER-16, which outlives it
  ARR-SET-INNER<Void → UFIX>{0} at a value of type REF UFIX, offset 1216
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

A store of a reference is rejected when what it points into is freed before the
destination is. Stack assignment decides when storage is freed[^freed]:

| Storage | Freed |
|---|---|
| A parameter's or a global's | After the function |
| What the result holds | By the caller |
| An array, `Cell` or list | When the function returns |
| A growable value | When the `match` branch that makes it ends, or when the function returns |

A reference points into everything it reaches: the arrays it is read from and
whatever those arrays hold[^inexact].

| Stored | Into | Result |
|---|---|---|
| A global | Anything | Accepted |
| A parameter | A local | Accepted |
| A parameter | The same parameter | Accepted |
| A parameter | Another parameter, or a global | Left for the caller to check |
| A local | A parameter or a global | Error |
| A local | A local in the same branch or an outer one | Accepted unless it points into a growable value that a branch inside the destination's frees |
| A growable value freed when a branch ends | A local made outside that branch | Error |

```lisp
(let ((outer (arr:new 1 (arr:ref base 0))))
  (if (== n 0)
      (let ((g (grow:new)))
        (grow:push! g 7)
        (arr:set! outer 0 (arr:ref g 0)))         ; error: g is freed when the branch ends
      (let ((inner (arr:new 1 0)))
        (arr:set! outer 0 (arr:ref inner 0)))))   ; accepted: inner lives until the function returns
```

A reference may not leave a `match` branch, a `catch` body or branch, or a
`protect` body when it points into a growable value that scope frees. Arrays, `Cell`s and lists made in a branch live
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
| `root-summary-bare-calls` | The calls whose arguments point only into parameters and globals, which may free their iteration's storage ([STACKS.md](STACKS.md#loops)) |
| `root-summary-slice-free-calls` | The calls whose arguments reach no growable value the function built, which may give back the slices their iteration took from a bank |

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

- A call result has one root, so a reference into its cell part is rejected
  when its growable part is freed:
  [#16](https://github.com/takeiteasy/sheaf/issues/16).
- The SBCL path does not run the check:
  [#8](https://github.com/takeiteasy/sheaf/issues/8).

[^inexact]: A value read out of an array gets the roots of the array, which
    bound its lifetime without naming where it points. A reference read out of
    an array and stored into another is rejected when anything the first array
    holds is freed before the destination.

[^freed]: A growable local is freed when its `let` ends, so a branch frees the
    ones it binds unless the result holds them, or the branch returns them
    to a `let` outside it. A call result that holds
    both a cell and a growable value has one root, which the limitation above
    describes.
