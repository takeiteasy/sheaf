# Memory model

Sheaf has no heap and no garbage collector. Every value lives on the native
stack, in static data, or on one of a few **data stacks**, and memory is
freed when the scope that owns it ends. The model follows
[Goose](https://github.com/aardappel/goose)[^goose]. Data stack assignment
runs ([STACKS.md](STACKS.md)); roots do not yet ([STATUS.md](STATUS.md)).

## Data stacks

A data stack is a large address-space reservation with a bump pointer. The
compiler works out how many a program needs.

| Rule | Effect |
|---|---|
| A growable value is always on top of its own stack | Growth is a pointer bump, and nothing ever moves |
| Lifetimes on one stack nest | Leaving a scope frees everything it allocated with one store |
| Stacks are reused once their owner's scope ends | A program needs few of them |

References into a growable value stay valid as it grows, since nothing
moves.

## Roots

Every reference has a static **root**: the variable whose scope bounds what
it points at. A reference may only be stored where its root outlives the
destination. The compiler infers roots per specialization, so a program
never writes a lifetime.

```lisp
(let ((outer (cell:new None)))
  (let ((inner (cell:new 0)))
    (cell:write! outer (Some inner))))   ; error: inner does not outlive outer
```

## Where a value is built

| Value | Built in |
|---|---|
| A function's result | The caller's destination |
| Any other allocation | The current function's scope |

A function returning a list builds that list straight into its caller's
storage, with no copy.

How sizes and references are written in types is in [PRELUDE.md](PRELUDE.md).

A recursive cycle builds everything in one region, owned by the call into the
cycle and freed when it returns. Nothing is rejected for keeping a list across a
recursive call, as Goose rejects it.

## Coalton's data

Coalton's immutable data reads the same whether it is copied or shared, so
Sheaf can treat it as plain values without changing what a program means.
The mutable types (`Cell`, `Vector`, `Hashtable`) stay references and obey
the root rule. Lists are cons cells in a pool, linked by relative
references.

## Limitations

- The meaning of `Cell`, `Vector` and `Hashtable` is open:
  [#8](https://todo.sr.ht/~takeiteasy/sheaf/8).
- Cells are linked by C pointers, not relative references:
  [#26](https://todo.sr.ht/~takeiteasy/sheaf/26).
- A loop that allocates on every iteration and never exits grows without
  bound, and so does a cycle's region until the call into it returns:
  [#11](https://todo.sr.ht/~takeiteasy/sheaf/11).

[^goose]: Goose's specification covers the full rules: data stacks in §1.2
    and §1.3, roots in §9.2, stack assignment in §10.3, and recursion in §7.8.
