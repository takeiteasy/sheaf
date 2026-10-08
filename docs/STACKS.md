# Stack assignment

`assign-stacks` gives every cell and growable value in a lowered program a data
stack, after Goose's recipe[^recipe]. Stacks are reused once their owner's scope
ends, and a returned value is built in its caller's destination.

```lisp
(let* ((ir (sheaf:lower (sheaf:defunctionalize (sheaf:monomorphize program 'my-package::main))))
       (assigned (sheaf:assign-stacks ir)))
  (sheaf:lowered-stack-count assigned)   ; => 3
  (sheaf:run-lowered assigned))          ; runs on simulated stacks
```

## Where a value is built

| Value | Stack |
|---|---|
| A cell the result holds | The function's destination, chosen by its caller |
| Any other cell | The function's own pool, taken on entry and freed on exit |
| A call's result | Nowhere if the callee builds no storage in its destination; otherwise like a cell |
| An array | By its size class: a growable as below, any other like a cell |
| A growable local | A stack of its own for the rest of its `let`; a returned one uses the destination |

The result holds a cell when its roots reach it[^reach]: directly, through the
fields of another cell, or through a value stored into an array or `Cell`
([ROOTS.md](ROOTS.md)).

```lisp
(define (pair a b) (Cons a (Cons b Nil)))        ; built in the caller's destination
(define (dropped a) (total (pair a (+ a 1))))    ; pair's list lives in dropped's pool
(define (twice a) (+ (dropped a) (dropped (+ a 1))))
```

Both calls in `twice` reach the same clone of `dropped`, so the pool takes the
same index twice. A stack index is the lowest one not in use where the value is
built.

## Contexts

A def is cloned for each context it is called in: the destination of its
result, the stacks in use on entry, and for a recursive cycle the region it
allocates in. Every index is then a constant, and a clone's name shows its
context.

```text
PAIR<UFIX * UFIX → LIST UFIX>{9}[dest 2 busy 0 1 2]
```

## Recursive cycles

A cycle allocates in one region and frees it when the call into the cycle
returns. The region is the destination of the call into the cycle, or a stack
that call takes for itself when the result holds no storage[^region].

## Results

| Slot | Holds |
|---|---|
| `ir-construct-stack` | The stack a cell is pushed to |
| `ir-call-destination` | The stack a storage-holding result is built on |
| `ir-call-region` | A region the call takes and frees |
| `ir-let-stack` | The stack a growable local owns |
| `ir-prim-stack` | The stack an array is pushed to |
| `ir-def-pool` | The pool a clone owns |
| `ir-def-destination` | The destination a clone's result is built on |
| `ir-def-source` | The def a clone was cloned from |
| `lowered-stack-count` | The number of stacks the program needs |

`check-assigned` signals an error unless every slot a program needs is filled.
`run-lowered` simulates the stacks and signals an error on a stack with two
owners, a cell pushed to a stack without one, or a cell read after its stack was
freed.

## Limitations

- Size classes are provisional: [#24](https://todo.sr.ht/~takeiteasy/sheaf/24).
- A cycle keeps what it allocates until the call into it returns:
  [#11](https://todo.sr.ht/~takeiteasy/sheaf/11).
- A growable local live across a call back into its own cycle is an error.

[^recipe]: Goose's specification covers stack assignment in §10.3 and result
    placement in §7.3.
[^reach]: Root summaries come from `root-summaries` with errors off, so a
    program root checking rejects still gets stacks. A function builds in its
    destination when its summary's result holds `:fresh`, and
    `root-summary-reached` names the locals that go there.
[^region]: A cycle of functions whose results hold no storage and that build
    none takes no region.
