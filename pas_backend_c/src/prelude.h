#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Rc;

struct Class {
    struct Rc* name;
};

struct Rc {
    void* resource;
    struct Class* class;

    int count;
};

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

    fprintf(stderr, "heap: allocated %4zu bytes at 0x%p\n", len, mem);
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

            fprintf(stderr, "heap: freed %4zu bytes at 0x%p\n", removed->len, removed->at);
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

static struct Rc* RcAlloc(size_t len) {
    struct Rc* rc = Alloc(sizeof(struct Rc));
    if (!rc) {
        abort();
    }

    rc->count = 1;
    rc->class = NULL;
    rc->resource = Alloc(len);

    return rc;
}

static void RcRetain(struct Rc* rc) {
    if (!rc || !rc->count) {
        abort();
    }

    rc->count += 1;
}

static void RcRelease(struct Rc* rc) {
    if (!rc || !rc->count) {
        abort();
    }

    if (rc->count > 1) {
        rc->count -= 1;
    } else {
        Free(rc->resource);
        Free(rc);
    }
}

// hack: define a struct identical to System.String so we can access its fields
struct InternalString {
    unsigned char* chars;
    int32_t len;
};

// implementations of System.pas builtins

static int32_t System_StrToInt(struct Rc* str_rc) {
    if (!str_rc || !str_rc->resource) {
        abort();
    }

    struct InternalString* str = str_rc->resource;
    int i = atoi((char*) str->chars);
    return (int32_t) i;
}

static struct Rc* System_IntToStr(int32_t i) {
    char buf[12];
    sprintf(buf, "%d", i);

    struct Rc* str_rc = RcAlloc(sizeof(struct InternalString));
    struct InternalString* str = (struct InternalString*) str_rc->resource;
    str->len = strlen(buf);
    str->chars = Alloc(str->len);
    memcpy(str->chars, buf, str->len);

    return str_rc;
}

static unsigned char* System_GetMem(int32_t len) {
    return (unsigned char*) Alloc(len);
}

static void System_FreeMem(unsigned char* mem) {
    Free(mem);
}

static void System_Write(struct Rc* str_rc) {
    if (!str_rc || !str_rc->resource) {
        abort();
    }

    struct InternalString* str = (struct InternalString*) str_rc->resource;
    char* chars = (char*) str->chars;

    printf("%.*s", (int) str->len, chars);
}

static void System_WriteLn(struct Rc* str_rc) {
    System_Write(str_rc);
    putchar('\n');
}

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
