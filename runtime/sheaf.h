/* sheaf.h: the runtime of a Sheaf program in C. */
#ifndef SHEAF_H
#define SHEAF_H

#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif

#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#ifndef SHEAF_STACK_SIZE
#define SHEAF_STACK_SIZE ((size_t)1 << 32)
#endif

#ifndef SHEAF_BANK_SPACE
#define SHEAF_BANK_SPACE ((size_t)1 << 45)
#endif

#ifndef SHEAF_SLICE_SIZE
#define SHEAF_SLICE_SIZE ((size_t)1 << 30)
#endif

#ifndef SHEAF_SLICE_KEEP
#define SHEAF_SLICE_KEEP ((size_t)1 << 16)
#endif

#ifdef __APPLE__
#define SHEAF_MADV_FREE MADV_FREE
#else
#define SHEAF_MADV_FREE MADV_DONTNEED
#endif

#if defined(__GNUC__) || defined(__clang__)
#define SHEAF_UNUSED __attribute__((unused))
#define SHEAF_NORETURN __attribute__((noreturn))
#else
#define SHEAF_UNUSED
#define SHEAF_NORETURN
#endif

typedef struct {
    char *base, *top, *limit;
    int index;
} sheaf_stack;

static SHEAF_UNUSED SHEAF_NORETURN void sheaf_exhausted(sheaf_stack *stack) {
    if (stack->index < 0)
        fprintf(stderr, "a bank slice exhausted\n");
    else
        fprintf(stderr, "data stack %d exhausted\n", stack->index);
    exit(1);
}

static SHEAF_UNUSED SHEAF_NORETURN void sheaf_overflow(void) {
    fprintf(stderr, "integer overflow\n");
    exit(1);
}

#define SHEAF_CHECKED(bits)                                                     \
    static SHEAF_UNUSED int##bits##_t sheaf_add_i##bits(int##bits##_t a, int##bits##_t b) { \
        int##bits##_t r;                                                        \
        if (__builtin_add_overflow(a, b, &r)) sheaf_overflow();                 \
        return r;                                                               \
    }                                                                           \
    static SHEAF_UNUSED int##bits##_t sheaf_sub_i##bits(int##bits##_t a, int##bits##_t b) { \
        int##bits##_t r;                                                        \
        if (__builtin_sub_overflow(a, b, &r)) sheaf_overflow();                 \
        return r;                                                               \
    }                                                                           \
    static SHEAF_UNUSED int##bits##_t sheaf_mul_i##bits(int##bits##_t a, int##bits##_t b) { \
        int##bits##_t r;                                                        \
        if (__builtin_mul_overflow(a, b, &r)) sheaf_overflow();                 \
        return r;                                                               \
    }

SHEAF_CHECKED(8)
SHEAF_CHECKED(16)
SHEAF_CHECKED(32)
SHEAF_CHECKED(64)

#define SHEAF_FIT(bits)                                                         \
    static SHEAF_UNUSED int##bits##_t sheaf_fit_i##bits(int64_t x) {            \
        if (x != (int##bits##_t)x) sheaf_overflow();                            \
        return (int##bits##_t)x;                                                \
    }

SHEAF_FIT(8)
SHEAF_FIT(16)
SHEAF_FIT(32)

static SHEAF_UNUSED void sheaf_reserve(sheaf_stack *stacks, int count) {
    for (int i = 0; i < count; i++) {
        void *base = mmap(NULL, SHEAF_STACK_SIZE, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANON | MAP_NORESERVE, -1, 0);
        if (base == MAP_FAILED) {
            fprintf(stderr, "cannot reserve data stack %d\n", i);
            exit(1);
        }
        stacks[i].base = stacks[i].top = base;
        stacks[i].limit = (char *)base + SHEAF_STACK_SIZE;
        stacks[i].index = i;
    }
}

static SHEAF_UNUSED void *sheaf_push(sheaf_stack *stack, size_t size, size_t align) {
    uintptr_t top = ((uintptr_t)stack->top + align - 1) & ~(uintptr_t)(align - 1);
    if (top + size > (uintptr_t)stack->limit)
        sheaf_exhausted(stack);
    stack->top = (char *)(top + size);
    return (void *)top;
}

static SHEAF_UNUSED uint64_t sheaf_index(uint64_t index, uint64_t length) {
    if (index >= length) {
        fprintf(stderr, "the index %llu is outside the array of %llu elements\n",
                (unsigned long long)index, (unsigned long long)length);
        exit(1);
    }
    return index;
}

static SHEAF_UNUSED void sheaf_range(uint64_t lo, uint64_t hi, uint64_t length) {
    if (lo > hi || hi > length) {
        fprintf(stderr, "the range %llu to %llu is outside the array of %llu elements\n",
                (unsigned long long)lo, (unsigned long long)hi, (unsigned long long)length);
        exit(1);
    }
}

static SHEAF_UNUSED SHEAF_NORETURN void sheaf_grew_under(sheaf_stack *stack) {
    if (stack->index < 0)
        fprintf(stderr, "a growable value on a bank slice grew under another value\n");
    else
        fprintf(stderr, "a growable value on stack %d grew under another value\n", stack->index);
    exit(1);
}

/* Room for SIZE more bytes after END, the end of a growable value, which must be on top of STACK. */
static SHEAF_UNUSED void *sheaf_grow(sheaf_stack *stack, void *end, size_t size) {
    if ((char *)end != stack->top)
        sheaf_grew_under(stack);
    if ((size_t)(stack->limit - stack->top) < size)
        sheaf_exhausted(stack);
    stack->top += size;
    return end;
}

/* Give back the elements of a growable value from NEW_END up to END, which must be on top of STACK. */
static SHEAF_UNUSED void sheaf_shrink(sheaf_stack *stack, void *end, void *new_end) {
    if ((char *)end != stack->top)
        sheaf_grew_under(stack);
    stack->top = new_end;
}

/* A bank hands out a slice of one reservation for each growable value built from it. */
typedef struct sheaf_slice {
    sheaf_stack stack;
    struct sheaf_slice *next;
} sheaf_slice;

typedef struct {
    sheaf_slice *head;
} sheaf_bank;

static char *sheaf_slices, *sheaf_fresh;
static sheaf_slice *sheaf_free_slices;

static SHEAF_UNUSED sheaf_stack *sheaf_bank_stack(sheaf_bank *bank) {
    sheaf_slice *slice = sheaf_free_slices;
    if (slice) {
        sheaf_free_slices = slice->next;
    } else {
        if (!sheaf_slices) {
            void *base = mmap(NULL, SHEAF_BANK_SPACE, PROT_READ | PROT_WRITE,
                              MAP_PRIVATE | MAP_ANON | MAP_NORESERVE, -1, 0);
            if (base == MAP_FAILED) {
                fprintf(stderr, "cannot reserve the bank space\n");
                exit(1);
            }
            sheaf_slices = sheaf_fresh = base;
        }
        if ((size_t)(sheaf_slices + SHEAF_BANK_SPACE - sheaf_fresh) < SHEAF_SLICE_SIZE) {
            fprintf(stderr, "bank space exhausted\n");
            exit(1);
        }
        slice = (sheaf_slice *)sheaf_fresh;
        sheaf_fresh += SHEAF_SLICE_SIZE;
    }
    slice->stack.base = slice->stack.top = (char *)(slice + 1);
    slice->stack.limit = (char *)slice + SHEAF_SLICE_SIZE;
    slice->stack.index = -1;
    slice->next = bank->head;
    bank->head = slice;
    return &slice->stack;
}

/* Return the pages of SLICE past the first SHEAF_SLICE_KEEP bytes to the OS. */
static void sheaf_slice_decommit(sheaf_slice *slice) {
    static uintptr_t page;
    if (!page)
        page = (uintptr_t)sysconf(_SC_PAGESIZE);
    uintptr_t from = (uintptr_t)slice + (SHEAF_SLICE_KEEP > sizeof *slice ? SHEAF_SLICE_KEEP : sizeof *slice);
    from = (from + page - 1) & ~(page - 1);
    uintptr_t to = ((uintptr_t)slice->stack.top + page - 1) & ~(page - 1);
    if (to > from)
        madvise((void *)from, to - from, SHEAF_MADV_FREE);
}

/* Give back the slices taken since MARK, the bank's head when its owner began. */
static SHEAF_UNUSED void sheaf_bank_release(sheaf_bank *bank, sheaf_slice *mark) {
    while (bank->head != mark) {
        sheaf_slice *slice = bank->head;
        bank->head = slice->next;
        sheaf_slice_decommit(slice);
        slice->next = sheaf_free_slices;
        sheaf_free_slices = slice;
    }
}

/* A hash table of UFix keys. Its slots follow it on its stack, each a key, a used flag and a value. */
typedef struct {
    sheaf_stack *stack;
    uint64_t count, capacity;
} sheaf_table;

#define SHEAF_TABLE_MIN 8

static SHEAF_UNUSED uint64_t sheaf_table_hash(uint64_t key) {
    key ^= key >> 30;
    key *= UINT64_C(0xbf58476d1ce4e5b9);
    key ^= key >> 27;
    key *= UINT64_C(0x94d049bb133111eb);
    return key ^ (key >> 31);
}

#define SHEAF_SLOT(slots, slot, i) ((uint64_t *)((slots) + (i) * (slot)))

/* The slot of KEY in a table of CAPACITY slots, or the empty slot where it belongs. */
static SHEAF_UNUSED uint64_t sheaf_table_probe(char *slots, size_t slot, uint64_t capacity, uint64_t key) {
    uint64_t i = sheaf_table_hash(key) & (capacity - 1);
    while (SHEAF_SLOT(slots, slot, i)[1] && SHEAF_SLOT(slots, slot, i)[0] != key)
        i = (i + 1) & (capacity - 1);
    return i;
}

/* An empty table that has room for HINT entries before it grows, pushed to ON after HEADER bytes of its owner's header. */
static SHEAF_UNUSED sheaf_table *sheaf_table_new(sheaf_stack *on, size_t header, size_t slot, size_t align, uint64_t hint) {
    uint64_t capacity = SHEAF_TABLE_MIN;
    while (capacity * 3 < hint * 4)
        capacity *= 2;
    sheaf_table *table = sheaf_push(on, header + capacity * slot, align);
    table->stack = on;
    table->count = 0;
    table->capacity = capacity;
    memset((char *)table + header, 0, capacity * slot);
    return table;
}

static SHEAF_UNUSED uint64_t sheaf_table_find(sheaf_table *table, char *slots, size_t slot, uint64_t key) {
    uint64_t i = sheaf_table_probe(slots, slot, table->capacity, key);
    return SHEAF_SLOT(slots, slot, i)[1] ? i : table->capacity;
}

/* Build a table twice the size above the old slots, then move it down so it stays on top of its stack. */
static SHEAF_UNUSED void sheaf_table_grow(sheaf_table *table, char *slots, size_t slot) {
    uint64_t old = table->capacity, capacity = old * 2;
    char *fresh = sheaf_grow(table->stack, slots + old * slot, capacity * slot);
    memset(fresh, 0, capacity * slot);
    for (uint64_t i = 0; i < old; i++) {
        uint64_t *from = SHEAF_SLOT(slots, slot, i);
        if (from[1])
            memcpy(SHEAF_SLOT(fresh, slot, sheaf_table_probe(fresh, slot, capacity, from[0])), from, slot);
    }
    memmove(slots, fresh, capacity * slot);
    sheaf_shrink(table->stack, fresh + capacity * slot, slots + capacity * slot);
    table->capacity = capacity;
}

/* The slot to write KEY's value to; the table grows first when it is three quarters full. */
static SHEAF_UNUSED uint64_t sheaf_table_insert(sheaf_table *table, char *slots, size_t slot, uint64_t key) {
    if (slots + table->capacity * slot != table->stack->top)
        sheaf_grew_under(table->stack);
    uint64_t i = sheaf_table_probe(slots, slot, table->capacity, key);
    if (SHEAF_SLOT(slots, slot, i)[1])
        return i;
    if ((table->count + 1) * 4 > table->capacity * 3) {
        sheaf_table_grow(table, slots, slot);
        i = sheaf_table_probe(slots, slot, table->capacity, key);
    }
    SHEAF_SLOT(slots, slot, i)[0] = key;
    SHEAF_SLOT(slots, slot, i)[1] = 1;
    table->count++;
    return i;
}

/* Remove KEY, shifting later entries of its probe run back so no tombstone is left. */
static SHEAF_UNUSED void sheaf_table_remove(sheaf_table *table, char *slots, size_t slot, uint64_t key) {
    if (slots + table->capacity * slot != table->stack->top)
        sheaf_grew_under(table->stack);
    uint64_t mask = table->capacity - 1, i = sheaf_table_probe(slots, slot, table->capacity, key);
    if (!SHEAF_SLOT(slots, slot, i)[1])
        return;
    table->count--;
    for (uint64_t j = (i + 1) & mask; SHEAF_SLOT(slots, slot, j)[1]; j = (j + 1) & mask) {
        uint64_t home = sheaf_table_hash(SHEAF_SLOT(slots, slot, j)[0]) & mask;
        if (((j - home) & mask) >= ((j - i) & mask)) {
            memcpy(SHEAF_SLOT(slots, slot, i), SHEAF_SLOT(slots, slot, j), slot);
            i = j;
        }
    }
    memset(SHEAF_SLOT(slots, slot, i), 0, slot);
}

static SHEAF_UNUSED SHEAF_NORETURN void sheaf_no_match(void) {
    fprintf(stderr, "no branch matches\n");
    exit(1);
}

#endif
