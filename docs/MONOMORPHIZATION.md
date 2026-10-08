# Monomorphization

`monomorphize` specializes every global a root reaches at the ground types
it is used at, and removes type-class dictionaries. The result has no type
variables and no dictionaries, and every class method call is a direct call
to an instance's method.

```lisp
(let* ((program (sheaf:compile-program "main.lisp"))
       (specialized (sheaf:monomorphize program 'my-package::main)))
  (sheaf:specialized-bindings specialized))
;; => a table of TWICE<UFIX → UFIX>, TWICE<F32 → F32>, ... to their IR
```

## Example

```lisp
(declare twice (Num :a => :a -> :a))
(define (twice x) (+ x x))
```

| | Captured | Specialized at UFix |
|---|---|---|
| Name | `twice` | `TWICE<UFIX → UFIX>` |
| Parameters | `dict`, `x` | `x` |
| Body | `(+ dict x x)` | A direct call to the `Num UFix` instance's `+`, specialized |

## Rules

| Case | Result |
|---|---|
| A global used at a type | One specialization per ground type, shared by every use |
| A class method | The instance's method, found from the dictionary's type, specialized |
| Dictionaries | Removed: parameters, arguments, bindings and superclass fields |
| A generic local binding | One clone per type it is used at; dropped if unused |
| A type variable nothing fixes | `Unit`, e.g. the element type in `(length Nil)` |
| A constructor | Kept as a reference, with a ground type |
| Polymorphic recursion | An error once a binding has more than 16 specializations in progress[^limit] |

The output is Coalton codegen IR: a `specialized` holds each
specialization's node, `specialized-origin` gives the global and ground type
it came from, and `specialized-root` names the root's specialization.
`check-monomorphic` checks that a `specialized` is ground, holds no
dictionary or class method, and passes Coalton's node typechecker.

Errors are `specialization-error`s, which carry the chain of specializations
that led to them, each with its type and source location.

## Limitations

- The standard library's code is Coalton's optimized IR, so specializations
  of it keep raw Lisp, and raw Lisp that uses a dictionary is an error:
  [#12](https://todo.sr.ht/~takeiteasy/sheaf/12),
  [#16](https://todo.sr.ht/~takeiteasy/sheaf/16).
- Programs compiled for SBCL are not monomorphized, so a program can run there
  and still be rejected here: [#17](https://todo.sr.ht/~takeiteasy/sheaf/17).

[^limit]: `*specialization-limit*`, as in Goose. A local binding has the same
    limit on its clones.
