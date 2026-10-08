# Memory model

Sheaf has no heap and no garbage collector. Every value lives on the native
stack, in static data, or on one of a few **data stacks**, and memory is
freed when the scope that owns it ends. The model follows
[Goose](https://github.com/aardappel/goose)[^goose]. None of it is
implemented yet; [STATUS.md](STATUS.md) says what runs.

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

## Coalton's data

Coalton's immutable data reads the same whether it is copied or shared, so
Sheaf can treat it as plain values without changing what a program means.
The mutable types (`Cell`, `Vector`, `Hashtable`) stay references and obey
the root rule. Lists are cons cells in a pool, linked by relative
references.

## Limitations

- How sizes and references are written in types is open:
  [#7](https://todo.sr.ht/~takeiteasy/sheaf/7).
- The meaning of `Cell`, `Vector` and `Hashtable` is open:
  [#8](https://todo.sr.ht/~takeiteasy/sheaf/8).
- Recursion that keeps growable locals across a recursive call is open:
  [#10](https://todo.sr.ht/~takeiteasy/sheaf/10).
- A loop that allocates on every iteration and never exits grows without
  bound: [#11](https://todo.sr.ht/~takeiteasy/sheaf/11).

[^goose]: Goose's specification covers the full rules: data stacks in §1.2
    and §1.3, roots in §9.2, stack assignment in §10.3, and recursion in §7.8.
