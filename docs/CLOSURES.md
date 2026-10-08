# Closures

Sheaf keeps Coalton's first-class functions, including closures that are
stored or returned, without a heap. After monomorphization, the compiler
**defunctionalizes**: each function type becomes a tagged union of the
lambdas that can reach it, and a call through a function value becomes a
`match` on the tag. This is planned; nothing is implemented yet.

```lisp
(define (make-counter)
  (let ((c (cell:new 0)))
    (fn () (cell:update! (fn (x) (+ x 1)) c))))
```

Here the returned function type has one possible lambda, so it becomes a
one-case union holding `c`, and calling it is a direct call.

| Coalton | After defunctionalization |
|---|---|
| A function type | A tagged union of the lambdas that reach it |
| A lambda's captures | The fields of its case |
| A call through a function value | A `match` on the tag; direct when there is one case |
| A `Cell` captured by one closure | A field of that closure |

A closure is then a plain value, built in its destination and checked by the
root rules in [MEMORY.md](MEMORY.md).

## Limitations

- The flow analysis that finds which lambdas reach which call is not built:
  [#2](https://todo.sr.ht/~takeiteasy/sheaf/2).
- A closure nested inside closures to a depth known only at runtime, such as
  an iterator rebuilt in a loop, would contain itself:
  [#3](https://todo.sr.ht/~takeiteasy/sheaf/3).
