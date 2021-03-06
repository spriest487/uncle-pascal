#if !NO_STDLIB

#define STRING_STRUCT struct Struct_1
#define STRING_CLASS Class_1
#define STRING_PTR(s_rc) ((STRING_STRUCT*) s_rc->resource)
#define STRING_CHARS(s_rc) (STRING_PTR(s_rc)->field_0)
#define STRING_LEN(s_rc) (STRING_PTR(s_rc)->field_1)

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

#endif