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

static struct Rc* RcAlloc(const void* val, size_t len) {
    struct Rc* rc = malloc(sizeof(struct Rc));
    if (!rc) {
        abort();
    }

    rc->count = 1;
    rc->class = NULL;
    rc->resource = malloc(len);
    memmove(rc->resource, val, len);

    return rc;
}

static void RcFree(struct Rc* rc) {
    if (!rc || !rc->count) {
        abort();
    }

    if (rc->count > 1) {
        rc->count -= 1;
    } else {
        free(rc->resource);
        free(rc);
    }
}

// hack: define a struct identical to System.String so we can access its fields
struct InternalString {
    unsigned char* chars;
    int32_t len;
};

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

    struct InternalString str;
    str.len = strlen(buf);
    str.chars = malloc(str.len);
    memcpy(str.chars, buf, str.len);

    struct Rc* str_rc = RcAlloc(&str, sizeof(str));
    return str_rc;
}

static unsigned char* System_GetMem(int32_t len) {
    unsigned char* mem = malloc((size_t) len);
    if (!mem) {
        abort();
    }

    return mem;
}

static void System_FreeMem(unsigned char* mem) {
    free(mem);
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
