# Sheaf IR

After defunctionalization, `lower` turns the program into Sheaf's own
first-order IR in A-normal form. Root checking ([ROOTS.md](ROOTS.md)), data stack assignment
([STACKS.md](STACKS.md)) and the C emitter ([EMITTER.md](EMITTER.md)) read this IR, so they never see Coalton's node
types or environment.

```lisp
(let* ((first-order (sheaf:defunctionalize (sheaf:monomorphize program 'my-package::main)))
       (ir (sheaf:lower first-order)))
  (sheaf:check-lowered ir)
  (sheaf:run-lowered ir))
```

## Example

```lisp
(define (area w h) (* (+ w 1) h))
```

```text
area(w, h):
  let t1 = prim ufix-add w 1
  let t2 = prim ufix-mul t1 h
  t2
```

Every operand is an atom: a literal, a local or a global. Everything else is
bound by a `let` in evaluation order, left to right.

## Nodes

| Node | Holds |
|---|---|
| `ir-literal`, `ir-local`, `ir-global` | The atoms; a global is a `:value` def |
| `ir-let` | A name, one simple expression, and the rest |
| `ir-call` | A global function and atom arguments |
| `ir-construct` | A constructor and atom field values |
| `ir-prim` | A primitive operation and atom arguments |
| `ir-foreign` | Raw Lisp and the locals it reads[^foreign] |
| `ir-match` | An atom and branches; a branch holds a pattern and a body |
| `ir-throw` | An atom, an exception to throw ([EXCEPTIONS.md](EXCEPTIONS.md)) |
| `ir-panic` | A message; unwinds to a `catch` of `_` ([EXCEPTIONS.md](EXCEPTIONS.md#panics)) |
| `ir-catch` | A body and branches; a branch holds an exception type and pattern, or neither, and a body |
| `ir-protect` | A body and a cleanup |
| `ir-loop` | Variables with initial atoms and steps, a test and which value of it exits, a body run for its effects, and a result |
| `ir-break`, `ir-continue` | A label; leave the loop, or go to its steps |
| `ir-block` | A label and a body; a return to the label ends it |
| `ir-return` | A label and an atom, the value of the block it leaves |

Patterns are `ir-pvar`, `ir-pwild`, `ir-plit`, `ir-pas` and `ir-pcon`. Every
node has a type and a source location; a node without one takes its
enclosing node's.

## Programs

| Part | Holds |
|---|---|
| `lowered-defs` | `ir-def`s: a `:function` with parameters, or a `:value`; each with its origin and location |
| `lowered-types` | The constructors of every data type reached, in tag order, with field types |
| `lowered-root` | The root's name |
| `lowered-stack-count` | The stacks the program needs, once assigned ([STACKS.md](STACKS.md)); otherwise `nil` |

A type is an `ir-type` of kind `:scalar`, `:data`, `:storage` (the prelude's
arrays, `Cell`, `Vector` and `Hashtable`) or `:view` (`Slice` and `Ref`), and
equal types are the same object.

| Function | Does |
|---|---|
| `lower` | Lowers a defunctionalized program |
| `check-lowered` | Signals an error unless names are bound, arities match and operands are atoms |
| `run-lowered` | Evaluates the root; defines what a lowered program means, on simulated stacks once assigned |
| `walk-ir` | Visits every node, outermost first |

## Primitives

`Num`, `Eq` and `Ord` methods of the numeric types ([NUMBERS.md](NUMBERS.md))
and `==` on `Ordering` become `ir-prim` and have no def. They are matched by
class, method and type, and the operation is `:add`, `:sub`, `:mul`,
`:from-int`, `:eq` or `:compare`; the type of the operands gives its meaning.
`fromInt` of a literal becomes a literal of the type.

The operations of the prelude ([PRELUDE.md](PRELUDE.md)) are `ir-prim`s as
well. An allocating one holds the stack it pushes to in `ir-prim-stack`.

## Limitations

- Raw Lisp that is not a known panic stays `ir-foreign`, which `run-lowered` and
  the emitter reject: [#48](https://github.com/takeiteasy/sheaf/issues/48).
- `Integral`, `Reciprocable`, `Bits` and `Bit` are not primitives:
  [#21](https://github.com/takeiteasy/sheaf/issues/21).
- A recursive `let` is an error: [#10](https://github.com/takeiteasy/sheaf/issues/10).
- A return, break or continue out of a `catch` or `protect` and a growable loop
  variable built anew by a step are errors:
  [#40](https://github.com/takeiteasy/sheaf/issues/40),
  [#41](https://github.com/takeiteasy/sheaf/issues/41).

[^foreign]: The standard library mixes raw Lisp with Coalton's own runtime
    representations, such as `Optional` and `Cell`, so a fragment cannot run
    on Sheaf's data. `counter.lisp` lowers but does not run for that reason.
