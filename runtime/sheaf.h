/* sheaf.h: the runtime of a Sheaf program in C. */
#ifndef SHEAF_H
#define SHEAF_H

#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

#ifndef SHEAF_STACK_SIZE
#define SHEAF_STACK_SIZE ((size_t)1 << 32)
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
    if (top + size > (uintptr_t)stack->limit) {
        fprintf(stderr, "data stack %d exhausted\n", stack->index);
        exit(1);
    }
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
    fprintf(stderr, "a growable value on stack %d grew under another value\n", stack->index);
    exit(1);
}

/* Room for SIZE more bytes after END, the end of a growable value, which must be on top of STACK. */
static SHEAF_UNUSED void *sheaf_grow(sheaf_stack *stack, void *end, size_t size) {
    if ((char *)end != stack->top)
        sheaf_grew_under(stack);
    if ((size_t)(stack->limit - stack->top) < size) {
        fprintf(stderr, "data stack %d exhausted\n", stack->index);
        exit(1);
    }
    stack->top += size;
    return end;
}

/* Give back the elements of a growable value from NEW_END up to END, which must be on top of STACK. */
static SHEAF_UNUSED void sheaf_shrink(sheaf_stack *stack, void *end, void *new_end) {
    if ((char *)end != stack->top)
        sheaf_grew_under(stack);
    stack->top = new_end;
}

static SHEAF_UNUSED SHEAF_NORETURN void sheaf_no_match(void) {
    fprintf(stderr, "no branch matches\n");
    exit(1);
}

#endif
