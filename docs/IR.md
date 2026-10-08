# Sheaf IR

After defunctionalization, `lower` turns the program into Sheaf's own
first-order IR in A-normal form. Root checking, data stack assignment and the C
emitter ([MEMORY.md](MEMORY.md)) read this IR, so they never see Coalton's node
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

Patterns are `ir-pvar`, `ir-pwild`, `ir-plit`, `ir-pas` and `ir-pcon`. Every
node has a type and a source location; a node without one takes its
enclosing node's.

## Programs

| Part | Holds |
|---|---|
| `lowered-defs` | `ir-def`s: a `:function` with parameters, or a `:value`; each with its origin and location |
| `lowered-types` | The constructors of every data type reached, in tag order, with field types |
| `lowered-root` | The root's name |

A type is an `ir-type` of kind `:scalar`, `:data` or `:reference` (`Cell`,
`Vector`, `Hashtable`), and equal types are the same object.

| Function | Does |
|---|---|
| `lower` | Lowers a defunctionalized program |
| `check-lowered` | Signals an error unless names are bound, arities match and operands are atoms |
| `run-lowered` | Evaluates the root; defines what a lowered program means |
| `walk-ir` | Visits every node, outermost first |

## Primitives

Instance methods in `*primitives*` become `ir-prim` and have no def: UFix
`+`, `-`, `*`, `==`, `<=>` and `fromInt`, and `==` on `Ordering`. `fromInt` of
a literal becomes a UFix literal.

## Limitations

- Raw Lisp in the standard library stays `ir-foreign`, which `run-lowered`
  and the emitter reject: [#12](https://todo.sr.ht/~takeiteasy/sheaf/12),
  [#16](https://todo.sr.ht/~takeiteasy/sheaf/16).
- Other numeric types and operations are not primitives:
  [#23](https://todo.sr.ht/~takeiteasy/sheaf/23).
- A recursive `let` or the node kinds defunctionalization rejects are
  errors: [#20](https://todo.sr.ht/~takeiteasy/sheaf/20).
- `Cell`, `Vector` and `Hashtable` are opaque references:
  [#8](https://todo.sr.ht/~takeiteasy/sheaf/8).
- `UFix` is unsigned 63-bit, and overflow is unspecified:
  [#9](https://todo.sr.ht/~takeiteasy/sheaf/9).

[^foreign]: The standard library mixes raw Lisp with Coalton's own runtime
    representations, such as `Optional` and `Cell`, so a fragment cannot run
    on Sheaf's data. `counter.lisp` lowers but does not run for that reason.
