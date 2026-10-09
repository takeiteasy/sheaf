# Stack assignment

`assign-stacks` gives every cell and growable value in a lowered program a data
stack, after Goose's recipe[^recipe]. Stacks are reused once their owner's scope
ends, and a returned value is built on stacks its caller gives the function.

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
| A call's result | Like a cell for what it builds in its destination, and a stack of its own for each growable value it returns |
| An array | By its size class[^class]: a growable as below, any other like a cell |
| A growable value the result holds | One of the function's growable stacks, one for each such value, chosen by its caller |
| A growable local | A stack of its own for the rest of its `let` |
| A growable value a recursive cycle builds at every level | A stack the run takes from a bank[^bank] |

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

A growable value stays on top of its stack so it can grow, so each one a result
holds gets a stack of its own[^count]:

```lisp
(define (two n)                                  ; two growable stacks, no destination
  (let ((a (vector:new)) (b (vector:new)))
    (vector:push! n a)
    (vector:push! n b)
    (Tuple a b)))
(define (with-list n)                            ; a destination and one growable stack
  (let ((v (vector:new)))
    (vector:push! n v)
    (Tuple v (Cons n Nil))))
```

## Contexts

A def is cloned for each context it is called in: the destination of its
result, the stacks of its growable values, the stacks in use on entry, and for
a recursive cycle the region it allocates in. Every index is then a constant,
and a clone's name shows its context.

```text
PAIR<UFIX * UFIX → LIST UFIX>{9}[dest 2 busy 0 1 2]
TWO<UFIX → TUPLE (VECTOR UFIX) (VECTOR UFIX)>{16}[grow 1 2 busy 0 1 2]
```

## Recursive cycles

A cycle allocates in one region and frees it when the call into the cycle
returns. The region is the destination of the call into the cycle, or a stack
that call takes for itself when the result holds no storage[^region].

## Banks

A cycle that builds a growable value at every level, such as `vectors-down`,
needs a number of stacks that only the run knows. Its result takes a bank in
place of growable stacks: the caller passes a stack index that names the bank,
and each growable value the cycle builds takes a stack of its own from it.

```lisp
(define (vectors-down n)                         ; a destination and a bank
  (let ((v (vector:new)))
    (if (== n 0) Nil (Cons v (vectors-down (- n 1))))))
```

The bank belongs to whoever owns the result: the `let` that drops it, or the
caller's own bank when the result reaches the caller's result. A callee that
builds growable values and is called by a banked def takes the bank too.

```text
VECTORS-DOWN<UFIX → LIST (VECTOR UFIX)>{12}[dest 1 bank 2 busy 0 1 2]
```

## Results

| Slot | Holds |
|---|---|
| `ir-construct-stack` | The stack a cell is pushed to |
| `ir-call-destination` | The stack the cells and arrays of a result are built on |
| `ir-call-growables` | The stacks the growable values of a result are built on |
| `ir-call-bank` | The bank a result's growable values are built from |
| `ir-call-region` | A region the call takes and frees |
| `ir-let-stacks` | The stacks the growable values a `let` binds own |
| `ir-let-bank` | The bank a `let` owns for a result it drops |
| `ir-prim-bank` | The bank a growable value takes its stack from |
| `ir-prim-stack` | The stack an array is pushed to |
| `ir-def-pool` | The pool a clone owns |
| `ir-def-destination` | The destination a clone's cells and arrays are built on |
| `ir-def-growables` | The stacks a clone's growable values are built on |
| `ir-def-bank` | The bank a clone's growable values are built from |
| `ir-def-source` | The def a clone was cloned from |
| `lowered-stack-count` | The number of stacks the program needs |

`check-assigned` signals an error unless every slot a program needs is filled.
`run-lowered` simulates the stacks and signals an error on a stack with two
owners, a cell pushed to a stack without one, or a cell read after its stack was
freed.

## Limitations

- In C a bank holds about 32,000 growable values at a time
  ([EMITTER.md](EMITTER.md#banks)).
- A cycle keeps what it allocates until the call into it returns:
  [#11](https://todo.sr.ht/~takeiteasy/sheaf/11).
- A growable local live across a call back into its own cycle is an error.

[^recipe]: Goose's specification covers stack assignment in §10.3 and result
    placement in §7.3.
[^reach]: Root summaries come from `root-summaries` with errors off, so a
    program root checking rejects still gets stacks. A function builds in its
    destination when its summary's result holds `:fresh`, and
    `root-summary-reached` names the locals that go there.
[^class]: `size-class` is `:fixed`, `:variable` or `:resizable`. The prelude
    types name theirs in `*prelude-types*` (prelude-ops.lisp), and any other
    type is `:fixed`.
[^count]: `compute-placements` counts the stacks a def's result needs, one for
    each growable site that reaches it and the callee's count for each call.
    Branches share stacks. A recursive cycle repeats until its counts stop
    changing.
[^bank]: A bank is a stack index. The interpreter hands out a new simulated
    stack for each growable value built from it and frees them all when the
    bank's owner ends. `compute-placements` marks a cycle banked when its
    growable count would grow with the recursion, and a def that reaches a
    banked call is banked too. C hands out slices of one reservation ([EMITTER.md](EMITTER.md#banks)).
[^region]: A cycle of functions whose results hold no storage and that build
    no cells or arrays takes no region. Growable values go on stacks of their
    own, so a cycle that builds only those takes none either.
