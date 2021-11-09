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

static struct Rc* System_IntToStr(int32_t i) {
    char buf[12];
    sprintf(buf, "%d", i);

    size_t len = strlen(buf);
    unsigned char* chars = Alloc(len);
    memcpy(chars, buf, len);

    struct Rc* str_rc = RcAlloc(&STRING_CLASS);
    STRING_LEN(str_rc) = len;
    STRING_CHARS(str_rc) = chars;

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

#endif