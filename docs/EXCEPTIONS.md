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

A handler saves the top of each data stack, and the head of each bank, that its
guarded expression can build on, and restores them when control lands in it.
Stack assignment records them (`ir-catch-marks`, `ir-protect-marks` and their
bank slots), and the stacks in use where the handler is entered
(`ir-catch-busy`, `ir-protect-busy`), which hold what the code after the handler
still reads[^busy].

| Where a throw leaves | After the unwind |
|---|---|
| A pool or region of an unwound call | Given back |
| A growable local, or a bank slice, of an unwound call | Given back |
| A growable value from outside the handler | Keeps what it grew |
| Cells and growable values built for the result in a stack in use | Given back[^reset] |

```lisp
(define (retry k)                       ; each pass gives back the cells that
  (if (== k 0)                          ; list-or-fail built before it threw
      (count-up 10 Nil)
      (catch (list-or-fail 100)
        ((Over _) (retry (- k 1))))))
```

## What an exception holds

A thrown value is copied, so an exception holds numbers and data built from them.
An exception that holds an array, slice, reference, `Cell` or growable value,
or a data type that contains itself such as `List`, is a lowering error naming
the type:

```text
Lowering does not support the exception type HEAVY, which holds VECTOR UFIX
```

## Panics

`assert`, `unreachable` and `error` with a string literal run on every tier. A
panic unwinds like a throw: `protect` cleanups run and `catch` with `_` takes
it, and no typed branch does. Unhandled, it prints its message to standard error
and exits with status 1.

| Coalton | Lowers to |
|---|---|
| `(assert d)` | A `match` on `d` whose `False` branch is an `ir-panic` of `Assertion <d> failed: ` |
| `(unreachable)` | An `ir-panic` of `Unreachable` |
| `(error "text")` | An `ir-panic` of `text` |

Any other raw Lisp stays `ir-foreign`. The interpreter rejects it and the C
emitter rejects it with the offset of the raw Lisp itself[^text].

## Limitations

- `handle`, `resumable` and `resume-to` are errors:
  [#24](https://github.com/takeiteasy/sheaf/issues/24).
- A return out of a `catch` or `protect` is an error:
  [#40](https://github.com/takeiteasy/sheaf/issues/40).
- `_` takes every exception a program throws and every panic. Overflow, an index
  outside an array, a `match` with no branch and an exhausted stack end the
  program.
- An `assert` with format arguments, an `error` whose message is not a string
  literal, and `unwrap` and `expect` are raw Lisp:
  [#48](https://github.com/takeiteasy/sheaf/issues/48).
- A handler whose guarded expression stores a value that holds storage keeps
  the cells it built in a stack in use:
  [#27](https://github.com/takeiteasy/sheaf/issues/27).
- An exception of `(repr :native ...)` has no constructors and is an error.

[^busy]: A stack is in use where a handler is entered if the function owns it,
    its caller gave it for the result, or a `let` around the handler owns it.
    Every call inside the guarded expression takes its own pools and regions
    from the others, so a stack outside that set holds nothing that survives
    the handler.
[^reset]: The stacks given back are the def's pool, region and destination, the
    growable stacks the handler's result is built on, and its result bank. A
    handler gives them back only if its guarded expression, a cleanup in it and
    every def they call store no value that holds storage and run no raw Lisp,
    since a stored value could outlive the unwind.
[^text]: The text of an `assert` is built when it is lowered, with the datum
    printed with package prefixes, so it can differ from the text SBCL prints.
