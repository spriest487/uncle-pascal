#ifdef _WIN32
    #define EXPORT_FN __declspec(dllexport)
#else
    #define EXPORT_FN
#endif

#include <stdio.h>
#include <stdint.h>

EXPORT_FN void X(void) {
    printf("native call to X()\n");
    fflush(stdout);
}

EXPORT_FN void Y(int32_t val) {
    printf("native call to Y(%d)\n", val);
    fflush(stdout);
}

EXPORT_FN int32_t Z() {
    return 123456;
}

static int int_value = 7654321;

EXPORT_FN int* ReturnsIntPtr() {
    return &int_value;
}

EXPORT_FN void PrintIntPtr(int* int_ptr) {
    printf("native call to PrintIntPtr(%d)\n", *int_ptr);
    fflush(stdout);
}