#include <inttypes.h>

#if _WIN32
#   define WIN32_LEAN_AND_MEAN
#   include <windows.h>
#else
#   include <dlfcn.h>
#endif

static void Raise(STRING_STRUCT* msg_str) {
    if (msg_str) {
        int32_t msg_len = STRING_LEN(msg_str);
        char* msg_chars = (char*) STRING_CHARS(msg_str);

        fprintf(stderr, "%.*s\n", (int) msg_len, msg_chars);
    }
    abort();
}

#if !NO_STDLIB

static int32_t System_StrToInt(STRING_STRUCT* str) {
    if (!str) {
        abort();
    }

    int i = atoi((char*) STRING_CHARS(str));
    return (int32_t) i;
}

#define INT_TO_STR_IMPL(FuncName, DataType, FormatString, BufSize) \
static STRING_STRUCT* FuncName(DataType i) { \
    char buf[BufSize]; \
    sprintf_s(buf, BufSize, "%" FormatString, i); \
    \
    size_t len = strlen(buf); \
    unsigned char* chars = Alloc(len); \
    memcpy(chars, buf, len); \
    \
    STRING_STRUCT* str = (STRING_STRUCT*) RcAlloc(&STRING_CLASS); \
    STRING_LEN(str) = len; \
    STRING_CHARS(str) = chars; \
    \
    return str; \
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

static void System_Write(STRING_STRUCT* str) {
    if (!str) {
        abort();
    }

    int len = (int) STRING_LEN(str);
    char* chars = (char*) STRING_CHARS(str);

    printf("%.*s", len, chars);
}

static void System_WriteLn(STRING_STRUCT* str) {
    System_Write(str);
    putchar('\n');
}

static STRING_STRUCT* System_ReadLn(void) {
    char buf[64];
    if (!fgets(buf, 64, stdin)) {
        fputs("ReadLn i/o failure\n", stderr);
        abort();
    }

    size_t len = strlen(buf);
    STRING_STRUCT* str = (STRING_STRUCT*) RcAlloc(&STRING_CLASS);
    STRING_LEN(str) = (int32_t) len;
    STRING_CHARS(str) = System_GetMem(len);
    memcpy(STRING_CHARS(str), buf, len);

    return str;
}

static int32_t System_ArrayLengthInternal(void* arr) {
    struct Rc* arr_rc = (struct Rc*) arr;
    if (!arr_rc) {
        abort();
    }

    struct DynArrayClass* array_class = (struct DynArrayClass*) arr_rc->class;

    return array_class->length(arr_rc);
}

static void* System_ArraySetLengthInternal(
    void* arr,
    int32_t new_len,
    void* default_val
) {
    struct Rc* arr_rc = (struct Rc*) arr;
    if (!arr_rc) {
        abort();
    }

    struct DynArrayClass* array_class = (struct DynArrayClass*) arr_rc->class;

    void* new_arr = RcAlloc(arr_rc->class);
    array_class->alloc(new_arr, new_len, arr_rc, default_val);

    return new_arr;
}

static void* LoadSymbol(const char* src, const char* sym) {
    void* sym_ptr = NULL;
#if _WIN32
    HINSTANCE lib = LoadLibraryA(src);
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