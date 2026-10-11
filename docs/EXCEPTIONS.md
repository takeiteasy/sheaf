# Exceptions

`throw`, `catch` and `protect` run on the interpreter and in C. A throw runs the
`protect` cleanups between it and the `catch` that takes it, and gives back the
data stacks the unwound calls were using.

```lisp
(define-exception Failure
  (Bad UFix)
  (Fatal))

(define (check n)
  (if (== n 0) (throw (Bad 5)) n))

(define (safe n)
  (let ((done (cell:new 0)))
    (catch (protect (check n) (cell:write! done 1))
      ((Bad x) (+ x 100))
      ((the Failure _) 7)
      (_ 0))))
```

## Forms

| Coalton | C |
|---|---|
| `throw e` | Copies `e` to the thrown value, then starts a throw |
| `catch` | A frame on the handler chain; the branch runs after the unwind |
| `protect` | A frame whose cleanup runs on a normal exit and on an unwind |

A `catch` branch is a constructor pattern, `(the T e)` for every exception of
type `T`, or `_`. The first branch that matches wins, and a branch runs in tail
position when the `catch` does.

## A throw

1. **Match.** The throw tests each `catch` branch, innermost frame first, before
   anything unwinds. With no match the program prints `unhandled exception T` and
   exits with status 1, running no cleanup.
2. **Unwind.** Control lands in each `protect` frame on the way, which runs its
   cleanup, then in the `catch` that matched, which runs its branch.

A throw from a cleanup replaces the one being unwound and goes to the frames
outside the `protect`.

## Stacks

A handler saves the top of every data stack that is free where it is entered,
and the head of every bank, and restores them when control lands in it.
Stack assignment records the stacks in use there (`ir-catch-busy`,
`ir-protect-busy`), which hold what the code after the handler still reads and
are left alone[^busy].

| Where a throw leaves | After the unwind |
|---|---|
| A pool or region of an unwound call | Given back |
| A growable local, or a bank slice, of an unwound call | Given back |
| A growable value from outside the handler | Keeps what it grew |
| Cells built for the result in a stack in use | Stay until that stack's owner ends |

## What an exception holds

A thrown value is copied, so an exception holds numbers and data built from them.
An exception that holds an array, slice, reference, `Cell` or growable value,
or a data type that contains itself such as `List`, is a lowering error naming
the type:

```text
Lowering does not support the exception type HEAVY, which holds VECTOR UFIX
```

## Limitations

- `handle`, `resumable` and `resume-to` are errors:
  [#24](https://github.com/takeiteasy/sheaf/issues/24).
- `_` takes every exception a program throws. Overflow, an index outside an array,
  a `match` with no branch and an exhausted stack end the program, and raw
  Lisp such as `unwrap` and `assert` is not translated
  ([#7](https://github.com/takeiteasy/sheaf/issues/7)).
- Cells a throw abandons in a stack in use stay until its owner ends:
  [#26](https://github.com/takeiteasy/sheaf/issues/26).
- A handler saves every free stack, however few the guarded expression uses:
  [#25](https://github.com/takeiteasy/sheaf/issues/25).
- An exception of `(repr :native ...)` has no constructors and is an error.

[^busy]: A stack is in use where a handler is entered if the function owns it,
    its caller gave it for the result, or a `let` around the handler owns it.
    Every call inside the guarded expression takes its own pools and regions
    from the others, so a stack outside that set holds nothing that survives
    the handler.
