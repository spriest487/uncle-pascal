#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

typedef int8_t System_Byte;
typedef int64_t System_Integer;
typedef void* System_Pointer;
typedef bool System_Boolean;

typedef struct System_String {
    System_Integer Rc;
    System_Byte* Chars;
    System_Integer Length;
} System_String;

static void System_WriteLn(System_String* line) {
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

static System_String* System_StringFromBytes(System_Byte* bytes, System_Integer len) {
    System_Byte* string = malloc((size_t)len);
    if (!string) {
        fputs("string allocation failed in System_StringFromBytes", stderr);
        abort();
    }

    memcpy(string, bytes, (size_t)len);

    System_String* result = malloc(sizeof(System_String));
    result->Rc = 1;
    result->Chars = string;
    result->Length = len;

    return result;
}