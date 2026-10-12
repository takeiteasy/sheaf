# Strings and characters

`Char` and `String` run on every tier: SBCL, the interpreter and C.

```lisp
(string:length "héllo")                          ; 5
(string:length (string:concat "ab" "é"))         ; 3
(char:char-code (string:ref-unchecked "aé" 1))   ; 233
(< "abc" "abd")                                  ; True
```

## Representation

| Type | In C | Notes |
|---|---|---|
| `Char` | `uint32_t` | A code point; compared like a number, with no arithmetic |
| `String` | `struct An *`, an array of `uint8_t` | UTF-8 bytes; immutable[^layout] |

A string literal is static data. A string built at run time (`concat`,
`substring`, a number converted to text) lives on a data stack like any other
array, so the root checks ([ROOTS.md](ROOTS.md)) and stack assignment
([STACKS.md](STACKS.md)) apply to it unchanged.

## Operations

| Function | Gives |
|---|---|
| `string:length`, `string:ref`, `string:ref-unchecked` | The number of characters, and a character by index[^count] |
| `string:substring`, `string:concat`, `<>` | A new string; `substring` takes its bounds in either order and clamps the larger to the length |
| `string:chars`, `string:strip-prefix`, `string:strip-suffix`, `string:split-at` | Built from the functions above |
| `==`, `<`, `<=>` and the rest of `Eq` and `Ord` on `String` | A comparison of the bytes, which orders by code point |
| `Into` from an integer type to `String` | The decimal text |
| `char:char-code`, `char:code-char` | A character's code, and the character a code names, if there is one |
| `char:ascii-alpha?`, `ascii-digit?`, `ascii-uppercase?`, `ascii-lowercase?`, `ascii-alphanumeric?`, `digit->ufix` | The ASCII classes[^replaced] |
| `==`, `<`, `<=>` on `Char` | As on numbers |

An index or range outside a string prints a message and exits in C and is an
error in the interpreter, as for arrays.

## Limitations

- `length`, `ref` and `substring` count code points by scanning:
  [#51](https://github.com/takeiteasy/sheaf/issues/51).
- The rest of the `String` and `Char` library is raw Lisp, which lowering
  rejects: [#50](https://github.com/takeiteasy/sheaf/issues/50).

[^layout]: A `String` has the layout of an `Array U8`: a `length` in bytes and
    the bytes, followed by a zero byte in a literal. The interpreter holds the
    same bytes.
[^count]: The index and length count code points, as Coalton does. The byte
    length is not exposed.
[^replaced]: The standard library's versions are raw Lisp, so the prelude
    replaces them with Coalton over `char-code`
    ([PRELUDE.md](PRELUDE.md#replacements)). `code-char` is `None` from
    `#x110000` up.
