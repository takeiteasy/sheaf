# C AST

The emitter ([EMITTER.md](EMITTER.md)) builds C as s-expressions and prints
them with `c-text` and `print-c`. The printer adds parentheses only where
precedence needs them.

```lisp
(sheaf::c-text '(:* (:+ (:name "a") (:name "b")) (:name "c")))   ; "(a + b) * c"
(sheaf::c-text '(:+ (:name "a") (:* (:name "b") (:name "c"))))   ; "a + b * c"
```

Identifiers, types and macros are strings. Everything else is a keyword node.

## Expressions and types

| Node | C |
|---|---|
| `(:name "x")`, `(:int 5)`, `(:string "a\n")` | `x`, `5`, `"a\n"` |
| `(:float 1.5f0)` | An exact hexadecimal float, with `f` for a single float; `NAN` and `INFINITY` |
| `(:ptr T)`, `(:struct "T0")` | `T *`, `struct T0` |
| `(:+ a b)`, `(:- …)`, `(:* …)`, `(:< …)`, `(:> …)`, `(:== …)`, `(:&& …)` | `a + b` and so on |
| `(:? c a b)` | `c ? a : b` |
| `(:neg x)`, `(:deref x)`, `(:addr x)`, `(:cast T x)` | `-x`, `*x`, `&x`, `(T)x` |
| `(:call "f" a …)`, `(:index x i)`, `(:-> x "m")`, `(:dot x "m")`, `(:post++ x)`, `(:post-- x)` | `f(a)`, `x[i]`, `x->m`, `x.m`, `x++`, `x--` |
| `(:compound T (".tag" e) (nil e) …)`, `(:init e …)` | `(T){ .tag = e, e }`, `{ e }` |
| `(:sizeof x)`, `(:alignof T)`, `(:offsetof T "m")` | `sizeof(x)`, `_Alignof(T)`, `offsetof(T, m)` |

## Statements

`print-c` writes a list of statements, indented, to a stream.

| Node | C |
|---|---|
| `(:decl T "x" init :unused t :static t)` | `static T x SHEAF_UNUSED = init;`; `init` is `nil` for none |
| `(:assign l r)`, `(:expr e)`, `(:return e)` | `l = r;`, `e;`, `return e;` |
| `(:if c then else)` | `if`, with an `if` as the whole `else` printed as `else if` |
| `(:for (:decl …) test step body…)` | A `for` loop |
| `(:block s…)`, `(:goto "l")`, `(:label "l")` | `{ … }`, `goto l;`, `l: ;` |
| `(:comment "t")`, `(:blank)` | `// t`, an empty line |

## Precedence

A child is parenthesized when it binds more loosely than its parent needs. The
right operand of a binary operator needs one level more, so `a - (b - c)` keeps
its brackets and `a - b - c` does not. A negative number or float is unary, so
`a - -5` has a space and `-(-5)` is parenthesized.
