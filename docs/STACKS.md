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
a recursive cycle the region it allocates in, and the spare region of a loop
that alternates. Every index is then a constant,
and a clone's name shows its context.

```text
PAIR<UFIX * UFIX → LIST UFIX>{9}[dest 2 busy 0 1 2]
TWO<UFIX → TUPLE (VECTOR UFIX) (VECTOR UFIX)>{16}[grow 1 2 busy 0 1 2]
```

## Recursive cycles

A cycle allocates in one region and frees it when the call into the cycle
returns. The region is the destination of the call into the cycle, or a stack
that call takes for itself when the result holds no storage[^region].

## Loops

A tail call back into its own cycle frees what its iteration built before it
jumps, so a loop that allocates on every iteration runs in bounded memory. It
does when its arguments point only into parameters and globals[^bare]:

```lisp
(define (scratch-loop n acc)                     ; each iteration's list is freed
  (if (== n 0)
      acc
      (scratch-loop (- n 1) (+ acc (total (Cons n (Cons 1 Nil)))))))
(define (build n acc)                            ; acc holds what the iteration built
  (if (== n 0) acc (build (- n 1) (Cons n acc))))
```

A call that carries what it built keeps the region, as `build` does.

A loop that drops what it was given and passes on what its iteration built
builds in two regions in turn[^alternate]:

```lisp
(define (rebuild n xs)                           ; builds in one region, frees the other
  (if (== n 0) (total xs) (rebuild (- n 1) (Cons n Nil))))
(define (thread n env xs)                        ; env is passed on unchanged
  (if (== n 0) (total xs) (thread (- n 1) env (Cons n (Cons (total env) Nil)))))
```

A loop does when each of its tail calls passes on only storage the iteration
built, globals and parameters every call back into the loop passes on unchanged,
and every call back into it is a tail call. A loop that keeps a changing
parameter along with what it built keeps both regions, as `accumulate` does:
[#33](https://github.com/takeiteasy/sheaf/issues/33). So does one with a `catch`
or `protect`: [#32](https://github.com/takeiteasy/sheaf/issues/32).

A loop that returns storage builds it in the two regions and copies the result
into the caller's destination when it returns[^copy]:

```lisp
(define (rebuild-list n xs)                      ; the list it returns is copied out
  (if (== n 0) xs (rebuild-list (- n 1) (Cons n Nil))))
```

A loop that builds growable values in a bank gives their slices back the same
way, when its arguments reach none of them[^slices]:

```lisp
(define (probe n acc)                            ; each iteration's vector is freed
  (let ((v (vector:new)))
    (vector:push! acc v)
    (if (== n 0) v (probe (- n 1) (+ acc (vector:length v))))))
```

A call that passes on a vector it built keeps its slices.

## `for` loops

A `for` loop keeps its variables in the function's frame. A growable local the
body builds is freed when its iteration ends, by falling through, by `continue`
or by `break`. The cells and arrays it builds go in the function's pool, and are
freed when the iteration ends if the loop's variables point only into
parameters, globals and what was built before the loop, and the iteration
stores no value that holds storage[^loop]:

```lisp
(for ((i 0 (+ i 1)))                             ; each iteration's list is freed
  :while (< i n)
  (let ((xs (Cons i (Cons 1 Nil))))
    (cell:write! total (+ (cell:read total) (len xs)))))

(for ((i 0 (+ i 1))
      (xs (the (List UFix) Nil) (Cons i xs)))    ; each cell stays: the variable holds it
  :returns (len xs)
  :while (< i n))
```

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
| `ir-call-spare` | The second region a call into a loop that alternates takes and frees |
| `ir-call-copy` | The stack a call into a loop that returns storage copies its result to |
| `ir-call-reset` | What a tail call frees before it jumps, each `(:stack index)` back to where its iteration began, `(:bank index)` likewise, or `(:spare index)`, the region the next iteration builds in, back to where it was when the loop was entered |
| `ir-let-stacks` | The stacks the growable values a `let` binds own |
| `ir-let-bank` | The bank a `let` owns for a result it drops |
| `ir-prim-bank` | The bank a growable value takes its stack from |
| `ir-prim-stack` | The stack an array is pushed to |
| `ir-def-pool` | The pool a clone owns |
| `ir-catch-busy`, `ir-protect-busy` | The stacks in use where a handler is entered ([EXCEPTIONS.md](EXCEPTIONS.md)) |
| `ir-catch-marks`, `ir-protect-marks` | The stacks a handler saves on entry and restores when control lands in it |
| `ir-catch-bank-marks`, `ir-protect-bank-marks` | The banks a handler saves and restores likewise |
| `ir-protect-stacks`, `ir-protect-bank` | The stacks and bank a `protect` cleanup owns |
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
- A cycle keeps what it allocates until the call into it returns, unless a
  tail call frees it each iteration:
  [#29](https://github.com/takeiteasy/sheaf/issues/29).
- A loop whose result holds a `Cell`, array or `Vector` keeps all it builds:
  [#35](https://github.com/takeiteasy/sheaf/issues/35).
- Copying a loop's result visits a shared cell once for each path to it and calls
  itself once for each cell on a path:
  [#34](https://github.com/takeiteasy/sheaf/issues/34).
- A loop that passes on both what it built and a changing parameter keeps all
  it builds: [#33](https://github.com/takeiteasy/sheaf/issues/33).
- A loop with a `catch` or `protect` keeps all it builds:
  [#32](https://github.com/takeiteasy/sheaf/issues/32).
- A `for` loop that carries a cell it built, or stores one outside itself, or
  handles exceptions, keeps what it builds until its function ends.
- A `for` variable holding a `Vector` or `Hashtable` cannot be built anew by a
  step: [#41](https://github.com/takeiteasy/sheaf/issues/41).
- A growable local live across a call back into its own cycle is an error:
  [#17](https://github.com/takeiteasy/sheaf/issues/17).

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
[^bare]: `root-summary-bare-calls` lists them. A tail call that jumps saves the
    region's top when the iteration starts and restores it before the jump
    ([EMITTER.md](EMITTER.md#calls)); the interpreter frees the cells above
    the mark, so a later read of one is an error.
[^slices]: `root-summary-slice-free-calls` lists the calls whose arguments reach
    only parameters, globals and values that hold no growable value. The
    iteration saves the bank's head when it starts and the jump gives back the
    slices taken since, as the owner of a bank does when it ends.
[^alternate]: `root-summary-call-roots` gives the roots each call's arguments
    reach, and `find-swaps` finds the calls that switch regions and the
    parameters that stay the same. The call into the loop takes two regions
    and clones the loop for each order of them, `REBUILD<…>[region 2 spare 1 …]`
    and `[region 1 spare 2 …]`. A call that switches regions frees the spare,
    which holds only what the previous iteration built, and jumps to the clone
    that builds in it. The interpreter frees the cells above what the spare held
    when it was taken, and C saves its top when the cycle is entered
    ([EMITTER.md](EMITTER.md#calls)).
[^copy]: The call into the loop builds in its own two regions, and the clone's
    destination is the region it builds in. Before the call frees them it copies
    each cell of the result on either region to the caller's destination, and
    shares every other. A result that holds a `Cell`, array or `Vector` is built
    in the destination as before.
[^region]: A cycle of functions whose results hold no storage and that build
    no cells or arrays takes no region. Growable values go on stacks of their
    own, so a cycle that builds only those takes none either.

[^loop]: The loop restores the pool's top to its value at the loop's start after
    each iteration's steps. A `break` leaves what its last iteration built.
