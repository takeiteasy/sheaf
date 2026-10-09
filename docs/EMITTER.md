# C emitter

`emit-c` writes a program with its data stacks assigned ([STACKS.md](STACKS.md))
as one C file. The file includes `runtime/sheaf.h`, which reserves the stacks.

```lisp
(let* ((ir (sheaf:lower (sheaf:defunctionalize (sheaf:monomorphize program 'my-package::main))))
       (assigned (sheaf:assign-stacks ir)))
  (sheaf:write-c assigned "main.c"))
```

```sh
cc -std=c11 -I runtime main.c -o main && ./main
```

The program prints its root's result when that is a `UFix`.

## Example

```lisp
(define (sum-to n acc)
  (if (== n 0)
      acc
      (sum-to (- n 1) (+ acc 1))))
```

```c
// SUM-TO<UFIX * UFIX → UFIX>{1}[busy 0]
static uint64_t sum_to_ufix___ufix___1(uint64_t n_60_5, uint64_t acc_61_6) {
  top: ;
  struct T0 t_7 = (struct T0){ .tag = n_60_5 == UINT64_C(0) ? 0 : 1 };
  if ((t_7).tag == 0) {
    return acc_61_6;
  } else if ((t_7).tag == 1) {
    uint64_t t_8 = (n_60_5 - UINT64_C(1));
    uint64_t t_9 = (acc_61_6 + UINT64_C(1));
    n_60_5 = t_8;
    acc_61_6 = t_9;
    goto top;
  } else {
    sheaf_no_match();
  }
}
```

Each function is one clone of a def, with its full name in a comment above it.
The excerpt omits `SHEAF_UNUSED` attributes and temporaries.

## Types

| IR type | C |
|---|---|
| `UFix` | `uint64_t` |
| A data type that contains itself | `struct Tn *`, a cell on a data stack[^links] |
| Any other data type | `struct Tn` by value: a `tag` and a union of the constructors' fields |
| `FixedArray`, `Array` | `struct An *`: a `length` and the elements[^layout] |
| `GrowArray`, `Stack`, `Vector` | `struct Gn *`: the stack it grows on, a `length` and the elements |
| `Slice` | `struct Sn` by value: a pointer to the first element and a `length` |
| `Ref`, `Cell` | A pointer to the element |
| `HashTable` | An emit error: [#32](https://todo.sr.ht/~takeiteasy/sheaf/32) |
| Any other scalar | An emit error |

## Stacks

| IR | C |
|---|---|
| A stack | An entry in `sheaf_stacks`, reserved with `mmap` at startup |
| A cell on stack `n` | `sheaf_push(&sheaf_stacks[n], …)` |
| A pool or region | The stack's `top` saved on entry and restored on exit |
| A growable local's stacks | The stacks' `top` saved before its `let` and restored after the body |
| A banked growable value | Pushed to a slice taken from the bank; a `let` that owns the bank gives its slices back after the body |
| A growable value | Pushed to its own stack and grown in place; an index or range outside it, or a push when another value is above it, prints a message and exits |

`SHEAF_STACK_SIZE` sets each stack's size (4 GiB by default, reserved, not
committed). A push past the end prints `data stack n exhausted` and exits.

## Banks

A bank ([STACKS.md](STACKS.md#banks)) is a `sheaf_bank`: a list of the slices it
has handed out. Each growable value built from it takes a slice of one
reservation on its first use, and the bank's owner gives them all back to a free
list when it ends.

| Setting | Default | Sets |
|---|---|---|
| `SHEAF_BANK_SPACE` | 32 TiB | The reservation, reserved with `MAP_NORESERVE` |
| `SHEAF_SLICE_SIZE` | 1 GiB | Each growable value's ceiling |

Depth is the space divided by the slice, about 32,000 banked values by default.
A slice past either limit prints `bank space exhausted` or `a bank slice exhausted` and exits.

## Calls

| Case | C |
|---|---|
| A call | A call of the clone's function |
| A self tail call | A jump to the top of the function |
| A global value | A function that computes it on first use |
| A `match` | An `if` chain on the tags, ending in `sheaf_no_match()` |

## Limitations

- Mutual tail calls grow the C stack:
  [#14](https://todo.sr.ht/~takeiteasy/sheaf/14).
- Exceptions, `unwind-protect` and restarts:
  [#13](https://todo.sr.ht/~takeiteasy/sheaf/13).
- Raw Lisp is an error: [#16](https://todo.sr.ht/~takeiteasy/sheaf/16),
  [#12](https://todo.sr.ht/~takeiteasy/sheaf/12).
- `HashTable`: [#32](https://todo.sr.ht/~takeiteasy/sheaf/32).
- A bank's freed slices stay committed:
  [#36](https://todo.sr.ht/~takeiteasy/sheaf/36).
- Only `UFix` and the data types built from it are emitted:
  [#23](https://todo.sr.ht/~takeiteasy/sheaf/23).
- `UFix` arithmetic wraps at 64 bits: [#9](https://todo.sr.ht/~takeiteasy/sheaf/9).
- Cells are linked by pointers, not relative references:
  [#26](https://todo.sr.ht/~takeiteasy/sheaf/26).

[^layout]: The elements follow the header at `offsetof(struct, e)`, so the first
    element is where the push started. `An`, `Gn` and `Sn` are shared by every
    array type with the same element type.
[^links]: A pointer into a data stack stays valid because nothing on a stack
    moves. [MEMORY.md](MEMORY.md) describes the smaller relative links the
    pointers will become.
