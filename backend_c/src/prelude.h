#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef void (*Disposer)(void*);

typedef void (*RcCleanupFunc)(void*);
typedef void (*RcRetainFunc)(void*);

#define STRING_STRUCT struct Struct_1
#define STRING_CLASS Class_1
#define STRING_CHARS(str_ptr) (str_ptr->field_0)
#define STRING_LEN(str_ptr) (str_ptr->field_1)

#ifdef _MSC_VER
#   define PACKED_DECL(DECL) __pragma(pack(push, 1)) DECL __pragma(pack(pop))
#else
#   define PACKED_DECL(DECL) DECL __attribute__((__packed__))
#endif

// classes and interfaces runtime support

struct MethodTable {
    size_t iface;
    struct MethodTable* next;
};

struct Class {
    size_t size;
    const char* name;

    struct MethodTable* iface_methods;

    RcCleanupFunc cleanup;
    Disposer disposer;
};

struct Rc {
    struct Class* class;
    int32_t strong_count;
    int32_t weak_count;
};

// forward decl of string type
STRING_STRUCT;

typedef void (*DynArrayAlloc)(void* arr, int32_t len, void* copy_from, void* default_val);
typedef int32_t (*DynArrayLength)(void* arr);

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

static void* RcAlloc(struct Class* class) {
    if (!class) {
        abort();
    }

    void* instance = Alloc(class->size);

    // the rc field should be the first member of any rc type
    struct Rc* rc = (struct Rc*)instance;
    rc->class = class;
    rc->strong_count = 1;
    rc->weak_count = 0;

    return rc;
}

static void RcRetain(void* instance) {
    if (!instance) {
        return;
    }

    struct Rc* rc = (struct Rc*)instance;
    if (rc->strong_count == 0) {
        fprintf(stderr, "called RcRetain for an invalid pointer\n");
        abort();
    }

    // don't retain immortal refs
    if (rc->strong_count < 0) {
        return;
    }
    
    rc->strong_count += 1;

#if TRACE_RC
    printf("rc: retain %s @ 0x%p (%d+%d refs)\n", rc->class->name, instance, rc->strong_count, rc->weak_count);
#endif
}

static void RcRelease(void* instance) {
    if (!instance) {
        // releasing NULL should be ignored
        return;
    }

    struct Rc* rc = (struct Rc*)instance;

    if (rc->strong_count == 0) {
        fprintf(stderr, "called RcRelease for an invalid pointer\n");
        abort();
    }

    if (rc->strong_count < 0) {
        // immortal
        return;
    }

#if TRACE_RC
    printf("rc: release %s @ 0x%p (%d+%d remain)\n", rc->class->name, instance, rc->strong_count - 1, rc->weak_count);
#endif

    if (rc->strong_count > 1) {
        // reference is still alive: don't free it yet 
        rc->strong_count -= 1;
        return;
    } 

    // run the disposer if present
    if (rc->class->disposer) {
#if TRACE_RC
        printf("rc:   disposing %s @ 0x%p\n", rc->class->name, instance);
#endif
        rc->class->disposer(instance);
        
        if (rc->strong_count > 1) {
            fprintf(stderr, "dispose function for %s added a reference to the disposed instance\n", rc->class->name);
            abort();
        }
    }
    
    rc->strong_count = 0;

    // invoke structural release to release struct fields
    rc->class->cleanup(instance);
    rc->class = NULL;

    // free memory
    Free(instance);
}

_Noreturn static void Raise(STRING_STRUCT* msg_str);

// implementations of System.pas builtins

static int32_t System_StrToInt(STRING_STRUCT* str);

static STRING_STRUCT* System_Int8ToStr(int8_t i);
static STRING_STRUCT* System_ByteToStr(uint8_t i);
static STRING_STRUCT* System_Int16ToStr(int16_t i);
static STRING_STRUCT* System_UInt16ToStr(uint16_t i);
static STRING_STRUCT* System_IntToStr(int32_t i);
static STRING_STRUCT* System_UInt32ToStr(uint32_t i);
static STRING_STRUCT* System_Int64ToStr(int64_t i);
static STRING_STRUCT* System_UInt64ToStr(uint64_t i);
static STRING_STRUCT* System_NativeIntToStr(ptrdiff_t i);
static STRING_STRUCT* System_NativeUIntToStr(size_t i);
static STRING_STRUCT* System_PointerToStr(const void* i);
static STRING_STRUCT* System_RealToStr(float f);

static unsigned char* System_GetMem(int32_t len);
static void System_FreeMem(unsigned char* mem);
static void System_Write(STRING_STRUCT* str);
static void System_WriteLn(STRING_STRUCT* str);
static STRING_STRUCT* System_ReadLn(void);
static int32_t System_ArrayLengthInternal(void* arr);
static void* System_ArraySetLengthInternal(void* arr, int32_t new_len, void* default_val);

static int32_t System_RandomInteger(int32_t from, int32_t to);
static float System_RandomSingle(float from, float to);

static float System_Pow(float val, float pow);
static float System_Sqrt(float val);
static float System_Sin(float val);
static float System_ArcSin(float val);
static float System_Cos(float val);
static float System_ArcCos(float val);
static float System_Tan(float val);
static float System_ArcTan(float val);

// runtime start/stop

// this needs to match what would ordinarily be generated for the System.String decl
PACKED_DECL(STRING_STRUCT {
    struct Rc rc;

    unsigned char* field_0;
    int32_t field_1;
});

static void ModuleInit();

static void* LoadSymbol(const char* src, const char* sym);

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
