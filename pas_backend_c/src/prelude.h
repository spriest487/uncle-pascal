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
