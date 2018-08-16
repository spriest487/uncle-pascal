#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

typedef int8_t System_Byte;
typedef int64_t System_Integer;
typedef void* System_Pointer;
typedef uint8_t System_Boolean;

struct System_Internal_RefCount {
    System_Integer StrongCount;
};

struct System_Internal_Rc {
    System_Pointer Value;
    struct System_Internal_RefCount* RefCount;
};

static struct System_Internal_Rc System_Internal_Rc_GetMem(System_Integer size, const char* constructorName) {
    struct System_Internal_Rc rc;
    rc.Value = malloc((size_t)size);
    if (!rc.Value) {
        fprintf(stderr, "object memory allocation failed in %s constructor\n", constructorName);
        abort();
    }

    rc.RefCount = malloc(sizeof(struct System_Internal_RefCount));
    if (!rc.RefCount) {
        fprintf(stderr, "rc memory allocation failed in %s constructor\n", constructorName);
        abort();
    }

    rc.RefCount->StrongCount = 0;

    fprintf(stderr, "rc allocated %lld bytes for %s\n", size, constructorName);
    return rc;
}

static void System_Internal_Rc_Retain(struct System_Internal_Rc* rc) {
    if (!rc->RefCount) {
        fprintf(stderr, "retained rc that was already deallocated @ %p + %p\n", rc->RefCount, rc->Value);
        abort();
    }

    rc->RefCount->StrongCount += 1;
}

static void System_Internal_Rc_Release(struct System_Internal_Rc* rc) {
    if (!rc->RefCount || rc->RefCount->StrongCount <= 0) {
        fprintf(stderr, "released rc that was already released @ %p + %p\n", rc->RefCount, rc->Value);
        abort();
    }

    rc->RefCount->StrongCount -= 1;
    if (rc->RefCount->StrongCount == 0) {
        fprintf(stderr, "rc deallocated @ %p + %p\n", rc->RefCount, rc->Value);

        free(rc->Value);
        free(rc->RefCount);
        rc->RefCount = NULL;
        rc->Value = NULL;
    }
}

struct System_String {
    System_Byte* Chars;
    System_Integer Length;
};

/* procedure System.WriteLn(line: System.String) */
static void System_WriteLn(struct System_Internal_Rc lineRc) {
    struct System_String* line = (struct System_String*)lineRc.Value;

    if (line) {
        for (int c = 0; c < line->Length; ++c) {
            fputc(line->Chars[c], stdout);
        }
        fputc('\n', stdout);
    } else {
        puts("");
    }
}

static System_Byte* System_GetMem(System_Integer bytes) {
    if (bytes > 0) {
        System_Byte* mem = malloc((size_t) bytes);
        if (!mem) {
            fputs("memory allocation failed\n", stderr);
            abort();
        }
        return mem;
    } else {
        return NULL;
    }
}

static void System_FreeMem(System_Byte* p) {
    free(p);
}

/* function System.StringFromBytes(chars: ^System.Byte; len: System.Integer): System.String */
static struct System_Internal_Rc System_StringFromBytes(System_Byte* bytes, System_Integer len) {
    System_Byte* string = malloc((size_t)len);
    if (!string) {
        fputs("string allocation failed in System_StringFromBytes\n", stderr);
        abort();
    }

    memcpy(string, bytes, (size_t)len);

    struct System_Internal_Rc result = System_Internal_Rc_GetMem(sizeof(struct System_String), "System.String");
    struct System_String* resultStr = (struct System_String*)result.Value;
    resultStr->Chars = string;
    resultStr->Length = len;

    return result;
}

/* function System.StringFromBytes(a: System.String; b: System.String): System.String */
static struct System_Internal_Rc System_StringConcat(struct System_Internal_Rc aRc, struct System_Internal_Rc bRc) {
    struct System_String* a = (struct System_String*)aRc.Value;
    struct System_String* b = (struct System_String*)bRc.Value;

    bool emptyA = !a || !a->Length;
    bool emptyB = !b || !b->Length;

    if (emptyA && emptyB) {
        return System_StringFromBytes((System_Byte*)"", 0);
    } else if (emptyA) {
        return System_StringFromBytes(b->Chars, b->Length);
    } else if (emptyB) {
        return System_StringFromBytes(a->Chars, a->Length);
    } else {
        System_Integer totalLength = a->Length + b->Length;
        System_Byte* chars = malloc((size_t)totalLength);
        if (!chars) {
            fputs("string allocation failed in System_StringConcat\n", stderr);
            abort();
        }

        for (System_Integer c = 0; c < a->Length; ++c) {
            chars[c] = a->Chars[c];
        }

        for (System_Integer c = 0; c < b->Length; ++c) {
            chars[c + a->Length] = b->Chars[c];
        }

        struct System_Internal_Rc result = System_StringFromBytes(chars, totalLength);
        free(chars);

        return result;
    }
}