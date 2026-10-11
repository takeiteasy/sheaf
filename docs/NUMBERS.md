# Numbers

The numeric types lower to `ir-prim`s and emit as C integers and floats. `+`,
`-`, `*`, `fromInt`, `==` and `<=>` are primitives; `<`, `>`, `<=`, `>=`, `min`
and `max` reach them through `<=>`.

| Type | C | On overflow |
|---|---|---|
| `U8`, `U16`, `U32`, `U64`, `UFix` | `uint8_t` … `uint64_t` | Wraps |
| `I8`, `I16`, `I32`, `I64`, `IFix`, `Integer` | `int8_t` … `int64_t` | Aborts[^trap] |
| `F32`, `F64` | `float`, `double` | IEEE 754 |

```lisp
(+ (the U8 250) 10)   ; 4
(+ (the I8 127) 1)    ; error: Overflow: 128 does not fit I8
```

The interpreter and the C program give the same result for every operation.

## Conversions

| `fromInt` of | Gives |
|---|---|
| A literal | A literal of the type; a literal a signed type cannot hold is a lowering error |
| A value, to an unsigned type | The value reduced to the width |
| A value, to a signed type | The value, or an overflow if it does not fit |
| A value, to a float | The nearest float |

## Comparison

`<=>` gives `LT`, `GT` or `EQ`. A comparison involving `NaN` gives `EQ`, and
`==` is false.

## Limitations

- `Integer` is 64 bits; a larger value is an overflow:
  [#19](https://github.com/takeiteasy/sheaf/issues/19).
- `Integral`, `Reciprocable`, `Bits` and `Bit` are not primitives:
  [#21](https://github.com/takeiteasy/sheaf/issues/21).
- A `HashTable` key must be a `UFix`:
  [#20](https://github.com/takeiteasy/sheaf/issues/20).

[^trap]: The interpreter signals `numeric-overflow`. The C program prints
    `integer overflow` to standard error and exits with status 1, through
    `__builtin_add_overflow` and its `sub` and `mul` forms, which gcc and clang
    provide. Coalton's `IFix` traps at the width of an SBCL fixnum, and Sheaf's
    at 64 bits. SBCL's own backend leaves `UFix` overflow unspecified, so a
    program that overflows a `UFix` runs differently there.
