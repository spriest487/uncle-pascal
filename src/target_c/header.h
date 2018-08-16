#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

typedef int8_t System_Byte;
typedef int64_t System_Integer;
typedef void* System_Pointer;
typedef uint8_t System_Boolean;

struct System_String {
    System_Byte* Chars;
    System_Integer Length;
};

static void System_WriteLn(struct System_String* line) {
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
            fputs("memory allocation failed", stderr);
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

static struct System_String* System_StringFromBytes(System_Byte* bytes, System_Integer len) {
    System_Byte* string = malloc((size_t)len);
    if (!string) {
        fputs("string allocation failed in System_StringFromBytes", stderr);
        abort();
    }

    memcpy(string, bytes, (size_t)len);

    struct System_String* result = malloc(sizeof(struct System_String));
    result->Chars = string;
    result->Length = len;

    return result;
}

static struct System_String* System_StringConcat(struct System_String* a, struct System_String* b) {
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
            fputs("string allocation failed in System_StringConcat", stderr);
            abort();
        }

        for (System_Integer c = 0; c < totalLength; ++c) {
            chars[c] = a->Chars[c];
        }

        for (System_Integer c = 0; c < totalLength; ++c) {
            chars[c + a->Length] = b->Chars[c];
        }

        struct System_String* result = System_StringFromBytes(chars, totalLength);
        free(chars);

        return result;
    }
}