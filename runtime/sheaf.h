/* sheaf.h: the runtime of a Sheaf program in C. */
#ifndef SHEAF_H
#define SHEAF_H

#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif

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

static SHEAF_UNUSED SHEAF_NORETURN void sheaf_no_match(void) {
    fprintf(stderr, "no branch matches\n");
    exit(1);
}

#endif
