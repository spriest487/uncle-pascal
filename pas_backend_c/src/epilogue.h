#include <inttypes.h>

#if _WIN32
#   define WIN32_LEAN_AND_MEAN
#   include <windows.h>
#else
#   include <dlfcn.h>
#endif

#define STRING_STRUCT struct Struct_1
#define STRING_CLASS Class_1
#define STRING_PTR(s_rc) ((STRING_STRUCT*) s_rc->resource)
#define STRING_CHARS(s_rc) (STRING_PTR(s_rc)->field_0)
#define STRING_LEN(s_rc) (STRING_PTR(s_rc)->field_1)

static void Raise(struct Rc* msg_str_rc) {
    if (msg_str_rc && msg_str_rc->resource) {
        int32_t msg_len = STRING_LEN(msg_str_rc);
        char* msg_chars = (char*) STRING_CHARS(msg_str_rc);

        fprintf(stderr, "%.*s\n", (int) msg_len, msg_chars);
    }
    abort();
}

#if !NO_STDLIB

static int32_t System_StrToInt(struct Rc* str_rc) {
    if (!str_rc || !str_rc->resource) {
        abort();
    }

    int i = atoi((char*) STRING_CHARS(str_rc));
    return (int32_t) i;
}

#define INT_TO_STR_IMPL(FuncName, DataType, FormatString, BufSize) \
static struct Rc* FuncName(DataType i) { \
    char buf[BufSize]; \
    sprintf(buf, "%" FormatString, i); \
    \
    size_t len = strlen(buf); \
    unsigned char* chars = Alloc(len); \
    memcpy(chars, buf, len); \
    \
    struct Rc* str_rc = RcAlloc(&STRING_CLASS); \
    STRING_LEN(str_rc) = len; \
    STRING_CHARS(str_rc) = chars; \
    \
    return str_rc; \
}

INT_TO_STR_IMPL(System_Int8ToStr, int8_t, PRId8, 4)
INT_TO_STR_IMPL(System_ByteToStr, uint8_t, PRIu8, 4)
INT_TO_STR_IMPL(System_Int16ToStr, int16_t, PRId16, 8)
INT_TO_STR_IMPL(System_UInt16ToStr, uint16_t, PRIu16, 8)
INT_TO_STR_IMPL(System_IntToStr, int32_t, PRId32, 12)
INT_TO_STR_IMPL(System_UInt32ToStr, uint32_t, PRIu32, 12)
INT_TO_STR_IMPL(System_Int64ToStr, int64_t, PRId64, 24)
INT_TO_STR_IMPL(System_UInt64ToStr, uint64_t, PRIu64, 24)
INT_TO_STR_IMPL(System_NativeIntToStr, ptrdiff_t, "zd", 24)
INT_TO_STR_IMPL(System_NativeUIntToStr, size_t, "zu", 24)

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

    int len = (int) STRING_LEN(str_rc);
    char* chars = (char*) STRING_CHARS(str_rc);

    printf("%.*s", len, chars);
}

static void System_WriteLn(struct Rc* str_rc) {
    System_Write(str_rc);
    putchar('\n');
}

static struct Rc* System_ReadLn(void) {
    char buf[64];
    if (!fgets(buf, 64, stdin)) {
        fputs("ReadLn i/o failure\n", stderr);
        abort();
    }

    size_t len = strlen(buf);
    struct Rc* str_rc = RcAlloc(&STRING_CLASS);
    STRING_LEN(str_rc) = (int32_t) len;
    STRING_CHARS(str_rc) = System_GetMem(len);
    memcpy(STRING_CHARS(str_rc), buf, len);

    return str_rc;
}

static int32_t System_ArrayLengthInternal(struct Rc* arr_rc) {
    if (!arr_rc || !arr_rc->resource) {
        abort();
    }

    struct DynArrayClass* array_class = (struct DynArrayClass*) arr_rc->class;

    return array_class->length(arr_rc);
}

static struct Rc* System_ArraySetLengthInternal(
    struct Rc* arr_rc,
    int32_t new_len,
    void* default_val,
    int32_t default_val_size
) {
    if (!arr_rc || !arr_rc->resource) {
        abort();
    }

    struct DynArrayClass* array_class = (struct DynArrayClass*) arr_rc->class;

    struct Rc* new_arr = RcAlloc(arr_rc->class);
    array_class->alloc(new_arr, new_len, arr_rc, default_val, default_val_size);

    return new_arr;
}

static void* LoadSymbol(const char* src, const char* sym) {
    void* sym_ptr = NULL;
#if _WIN32
    HINSTANCE lib = LoadLibrary(src);
    if (lib) {
        sym_ptr = GetProcAddress(lib, sym);
    }
#else
    void* lib = dlopen(src, RTLD_LAZY);
    if (lib) {
        sym_ptr = dlsym(lib, sym);
    }
#endif

    if (!sym_ptr) {
        fprintf(stderr, "failed to load symbol: %s::%s\n", src, sym);
        abort();
    }

    return sym_ptr;
}

#endif