# Closures

Sheaf keeps Coalton's first-class functions, including closures that are
stored or returned, without a heap. After monomorphization, `defunctionalize`
replaces each function type with a tagged union of the lambdas that can reach
it, and each call through a function value with a `match` on the tag.

```lisp
(let* ((program (sheaf:compile-program "main.lisp"))
       (specialized (sheaf:monomorphize program 'my-package::main))
       (first-order (sheaf:defunctionalize specialized)))
  (sheaf:check-defunctionalized first-order))
```

## Example

```lisp
(define (adder n) (fn (x) (+ x n)))

(define (pick n)
  (if (> n 5) (adder n) double))
```

`pick` returns a function type that two things reach: the lambda in `adder`
and the global `double`. The type becomes a union of two cases, and a call
through it becomes a `match`:

| | Before | After |
|---|---|---|
| Type of `pick` | `UFix → UFix → UFix` | `UFix → FN2` |
| `FN2` | | `FN2/L0 UFix`, the lambda holding `n`; `FN2/L1`, `double` |
| `((pick 10) 1)` | A call of a function value | `(match (pick 10) (FN2/L0 n) (adder/L0 n 1) (FN2/L1) (double 1))` |

A function type reached by one lambda with no captures is a one-case union, and
a call through it is a direct call.

## Rules

| Case | Result |
|---|---|
| A function type | A union with one case per lambda that reaches it[^sets] |
| A lambda | The constructor of its case, applied to its captures; its body becomes a global taking the captures, then the arguments |
| A global used as a value | A case with no captures |
| A call through a function value | A `match` on the tag; a direct call when the union has one case and no captures |
| A higher-order function | One specialization per combination of lambda sets it is used with |
| A data type with a function in a field, such as `Iterator` | One type per combination of lambda sets, with the fields' types replaced |
| A function as a type argument, such as `Optional (UFix → UFix)` | The same type applied to the union |
| A recursive local function | Not captured by its own lambda; rebuilt in its body from the captures |
| A closure nested inside closures to a runtime depth | An error naming the binding that nests it[^nested] |
| A function reaching raw Lisp | An error naming the value and its source location |

A closure is then a plain value, built in its destination and checked by the
root rules in [MEMORY.md](MEMORY.md).

## Result

`defunctionalize` returns a `specialized`, like `monomorphize`, holding only
the specializations the root reaches. A prelude primitive ([PRELUDE.md](PRELUDE.md))
that the program calls is kept as it was, with what it refers to, so the program
still runs on Coalton's backend; lowering replaces its calls.

| Function | Returns |
|---|---|
| `defunctionalize` | The program with no function values |
| `check-defunctionalized` | `T` if the program holds no lambda but each binding's own and no function-typed node but a called global, and passes Coalton's node typechecker |
| `specialized-type-definitions` | The unions and data types it synthesized |
| `specialized-environment` | Coalton's environment extended with those types |
| `recursive-types` | The synthesized types that contain themselves, such as a declared type with a function field[^recursive] |

Errors are `specialization-error`s with the specialization and source location
of the construct.

## Limitations

- A closure nested inside closures to a depth known only at runtime, such as
  an iterator rebuilt in a loop, is an error:
  [#11](https://github.com/takeiteasy/sheaf/issues/11).
- A function value passed to a standard library function that has no captured
  code, such as the list `fold` reached through `map`, is an error:
  [#3](https://github.com/takeiteasy/sheaf/issues/3).
- Partial application, keyword arguments, constructors used as functions and
  the `catch`, `for` and `values` forms are errors:
  [#10](https://github.com/takeiteasy/sheaf/issues/10).
- A `Cell` captured by only one closure stays a separate value, not a field of
  that closure's case:
  [#9](https://github.com/takeiteasy/sheaf/issues/9).

[^sets]: A lambda set is found by unifying function types: a function type's
    set holds the lambdas that reach it, and sets merge where values flow
    together. Each binding is analyzed once; a use of it sees a fresh copy of
    its sets, so `next!` called on two iterators with different closures gives
    two instances, not one merged set. A binding in a recursive group sees its
    group's sets unchanged.

[^nested]: `(set it (map f it))` in a loop gives an iterator whose closure
    captures an iterator of the same type, so the type would hold itself by
    value. The check follows captures, data fields and type arguments, and
    stops at `List`, `Cell`, `Vector` and `Hashtable`, which are linked or
    referenced.

[^recursive]: A type that holds itself through data fields alone, such as
    `Link (UFix → UFix) Chain`, is allowed here. It has no fixed size, so the
    memory model rejects it later.
