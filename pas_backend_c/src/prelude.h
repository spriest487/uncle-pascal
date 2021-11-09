#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Rc;

typedef void (*Disposer)(struct Rc*);

typedef void (*RcCleanupFunc)(void*);
typedef void (*RcRetainFunc)(void*);

// classes and interfaces runtime support

struct MethodTable {
    size_t iface;
    struct MethodTable* next;
};

struct Class {
    size_t size;

    struct MethodTable* iface_methods;

    RcCleanupFunc cleanup;
    Disposer disposer;
};

struct Rc {
    void* resource;
    struct Class* class;

    int count;
};

typedef void (*DynArrayAlloc)(struct Rc* arr, int32_t len, struct Rc* copy_from, void* default_val, int32_t default_val_len);
typedef int32_t (*DynArrayLength)(struct Rc* arr);

struct DynArrayClass {
    struct Class base;

    DynArrayAlloc alloc;
    DynArrayLength length;
};

static bool IsImpl(struct Class* class, size_t iface) {
    struct MethodTable* next = class->iface_methods;
    while (next) {
        if (next->iface == iface) {
            return true;
        }
        next = next->next;
    }

    return false;
}

// internal memory allocation

#ifdef TRACE_HEAP

    struct AllocTrace {
        void* at;
        size_t len;
        struct AllocTrace* next;
    };

    static struct AllocTrace* alloc_traces;

#endif

static void* Alloc(size_t len) {
    void* mem = calloc((size_t) len, 1);
    if (!mem) {
        abort();
    }

#ifdef TRACE_HEAP
    struct AllocTrace* new_alloc = malloc(sizeof(struct AllocTrace));
    new_alloc->next = alloc_traces;
    new_alloc->len = len;
    new_alloc->at = mem;
    alloc_traces = new_alloc;

    fprintf(stderr, "heap: alloc %4zu bytes at 0x%p\n", len, mem);
#endif

    return mem;
}

static void Free(void* mem) {
#ifdef TRACE_HEAP
    struct AllocTrace** alloc = &alloc_traces;

    while (*alloc) {
        if ((*alloc)->at == mem) {
            struct AllocTrace* removed = *alloc;
            *alloc = removed->next;

            fprintf(stderr, "heap:  free %4zu bytes at 0x%p\n", removed->len, removed->at);
            free(removed);
            break;
        } else {
            alloc = &((*alloc)->next);
        }
    }
#endif

    free(mem);
}

// RC runtime functions

static struct Rc* RcAlloc(struct Class* class) {
    if (!class) {
        abort();
    }

    struct Rc* rc = Alloc(sizeof(struct Rc));
    if (!rc) {
        abort();
    }

    rc->count = 1;
    rc->class = class;
    rc->resource = Alloc(class->size);

    return rc;
}

static void RcRetain(struct Rc* rc) {
    if (!rc || !rc->count) {
        abort();
    }

    // don't retain immortal refs
    if (rc->count < 0) {
        return;
    }

#if TRACE_RC
    printf("rc: retained ref @ 0x%p\n", rc->resource);
#endif

    rc->count += 1;
}

static void RcRelease(struct Rc* rc) {
    if (!rc) {
        // releasing NULL should be ignored
        return;
    }

    if (!rc->count) {
        abort();
    }

    if (rc->count < 0) {
        // immortal
        return;
    }

    if (rc->count > 1) {
        rc->count -= 1;

#if TRACE_RC
        printf("rc: released ref @ 0x%p\n", rc->resource);
#endif
    } else {
        // run the disposer if present
        if (rc->class->disposer) {
#if TRACE_RC
            printf("rc: deleting disposable resource @ 0x%p\n", rc->resource);
#endif
            rc->class->disposer(rc);
        } else {
#if TRACE_RC
            printf("rc: deleting resource without disposer @ 0x%p\n", rc->resource);
#endif
        }

        // invoke structural release to release struct fields
        rc->class->cleanup(rc->resource);

        // free memory
        Free(rc->resource);
        Free(rc);
    }
}

static void Raise(struct Rc* msg_str_rc);

#if !NO_STDLIB

// implementations of System.pas builtins

static int32_t System_StrToInt(struct Rc* str_rc);
static struct Rc* System_IntToStr(int32_t i);
static unsigned char* System_GetMem(int32_t len);
static void System_FreeMem(unsigned char* mem);
static void System_Write(struct Rc* str_rc);
static void System_WriteLn(struct Rc* str_rc);
static struct Rc* System_ReadLn(void);
static int32_t System_ArrayLengthInternal(struct Rc* arr_rc);
static struct Rc* System_ArraySetLengthInternal(struct Rc* arr_rc, int32_t new_len, void* default_val, int32_t default_val_len);

// Strings

// this needs to match what would ordinarily be generated for the System.String decl
struct Struct_1 {
    unsigned char* field_0;
    int32_t field_1;
};

#endif

// runtime start/stop

void ModuleInit();

static void RuntimeExit(int code) {
#if TRACE_HEAP
    struct AllocTrace* leaked_alloc = alloc_traces;
    while (leaked_alloc) {
        fprintf(stderr, "heap: leaked allocation of length %4zu at 0x%p\n", leaked_alloc->len, leaked_alloc->at);
        leaked_alloc = leaked_alloc->next;
    }
#endif

    exit(code);
}

int main() {
    ModuleInit();
    RuntimeExit(0);
}
