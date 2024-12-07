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

#define TYPEINFO_STRUCT struct Struct_2
#define TYPEINFO_NAME(typeinfo) (typeinfo->field_0)
#define TYPEINFO_NAME_CHARS(typeinfo) (TYPEINFO_NAME(typeinfo)->field_0)

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
    
    TYPEINFO_STRUCT* typeinfo;

    struct MethodTable* iface_methods;

    RcCleanupFunc cleanup;
    Disposer disposer;
};

struct Rc {
    struct Class* class;
    int32_t strong_count;
    int32_t weak_count;
};

// forward decl of builtin types
STRING_STRUCT;
TYPEINFO_STRUCT;

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

static void* Alloc(size_t len);
static void Free(void* mem);

// RC runtime functions

static void* RcAlloc(struct Class* class);
static void RcRetain(void* instance, bool weak);
static void RcRelease(void* instance, bool weak);

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

static float System_Infinity(void);
static bool System_IsInfinite(float val);
static float System_NaN(void);
static bool System_IsNaN(float val);

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
