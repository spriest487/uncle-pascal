#ifdef _WIN32
#define EXPORT_FN __declspec(dllexport)
#else
#define EXPORT_FN
#endif

#include <stdio.h>

EXPORT_FN void X(void) {
    printf("native call to X()\n");
}