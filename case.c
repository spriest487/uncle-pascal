#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Rc;

typedef void (*Disposer)(struct Rc*);

typedef void (*RcCleanupFunc)(void*);
typedef void (*RcRetainFunc)(void*);

// classes and interfaces runtime support

struct MethodTable {
    size_t iface;
    struct MethodTable* next;
};

struct Class {
    size_t size;

    struct MethodTable* iface_methods;

    RcCleanupFunc cleanup;
    Disposer disposer;
};

struct Rc {
    void* resource;
    struct Class* class;

    int count;
};

typedef void (*DynArrayAlloc)(struct Rc* arr, int32_t len, struct Rc* copy_from, void* default_val, int32_t default_val_len);
typedef int32_t (*DynArrayLength)(struct Rc* arr);

struct DynArrayClass {
    struct Class base;

    DynArrayAlloc alloc;
    DynArrayLength length;
};

static bool IsImpl(struct Class* class, size_t iface) {
    struct MethodTable* next = class->iface_methods;
    while (next) {
        if (next->iface == iface) {
            return true;
        }
        next = next->next;
    }

    return false;
}

// internal memory allocation

#ifdef TRACE_HEAP

    struct AllocTrace {
        void* at;
        size_t len;
        struct AllocTrace* next;
    };

    static struct AllocTrace* alloc_traces;

#endif

static void* Alloc(size_t len) {
    void* mem = calloc((size_t) len, 1);
    if (!mem) {
        abort();
    }

#ifdef TRACE_HEAP
    struct AllocTrace* new_alloc = malloc(sizeof(struct AllocTrace));
    new_alloc->next = alloc_traces;
    new_alloc->len = len;
    new_alloc->at = mem;
    alloc_traces = new_alloc;

    fprintf(stderr, "heap: alloc %4zu bytes at 0x%p\n", len, mem);
#endif

    return mem;
}

static void Free(void* mem) {
#ifdef TRACE_HEAP
    struct AllocTrace** alloc = &alloc_traces;

    while (*alloc) {
        if ((*alloc)->at == mem) {
            struct AllocTrace* removed = *alloc;
            *alloc = removed->next;

            fprintf(stderr, "heap:  free %4zu bytes at 0x%p\n", removed->len, removed->at);
            free(removed);
            break;
        } else {
            alloc = &((*alloc)->next);
        }
    }
#endif

    free(mem);
}

// RC runtime functions

static struct Rc* RcAlloc(struct Class* class) {
    if (!class) {
        abort();
    }

    struct Rc* rc = Alloc(sizeof(struct Rc));
    if (!rc) {
        abort();
    }

    rc->count = 1;
    rc->class = class;
    rc->resource = Alloc(class->size);

    return rc;
}

static void RcRetain(struct Rc* rc) {
    if (!rc || !rc->count) {
        abort();
    }

    // don't retain immortal refs
    if (rc->count < 0) {
        return;
    }

#if TRACE_RC
    printf("rc: retained ref @ 0x%p\n", rc->resource);
#endif

    rc->count += 1;
}

static void RcRelease(struct Rc* rc) {
    if (!rc) {
        // releasing NULL should be ignored
        return;
    }

    if (!rc->count) {
        abort();
    }

    if (rc->count < 0) {
        // immortal
        return;
    }

    if (rc->count > 1) {
        rc->count -= 1;

#if TRACE_RC
        printf("rc: released ref @ 0x%p\n", rc->resource);
#endif
    } else {
        // run the disposer if present
        if (rc->class->disposer) {
#if TRACE_RC
            printf("rc: deleting disposable resource @ 0x%p\n", rc->resource);
#endif
            rc->class->disposer(rc);
        } else {
#if TRACE_RC
            printf("rc: deleting resource without disposer @ 0x%p\n", rc->resource);
#endif
        }

        // invoke structural release to release struct fields
        rc->class->cleanup(rc->resource);

        // free memory
        Free(rc->resource);
        Free(rc);
    }
}

static void Raise(struct Rc* msg_str_rc);

#if !NO_STDLIB

// implementations of System.pas builtins

static int32_t System_StrToInt(struct Rc* str_rc);
static struct Rc* System_IntToStr(int32_t i);
static unsigned char* System_GetMem(int32_t len);
static void System_FreeMem(unsigned char* mem);
static void System_Write(struct Rc* str_rc);
static void System_WriteLn(struct Rc* str_rc);
static struct Rc* System_ReadLn(void);
static int32_t System_ArrayLengthInternal(struct Rc* arr_rc);
static struct Rc* System_ArraySetLengthInternal(struct Rc* arr_rc, int32_t new_len, void* default_val, int32_t default_val_len);

// Strings

// this needs to match what would ordinarily be generated for the System.String decl
struct Struct_1 {
    unsigned char* field_0;
    int32_t field_1;
};

#endif

// runtime start/stop

void ModuleInit();

static void RuntimeExit(int code) {
#if TRACE_HEAP
    struct AllocTrace* leaked_alloc = alloc_traces;
    while (leaked_alloc) {
        fprintf(stderr, "heap: leaked allocation of length %4zu at 0x%p\n", leaked_alloc->len, leaked_alloc->at);
        leaked_alloc = leaked_alloc->next;
    }
#endif

    exit(code);
}

int main() {
    ModuleInit();
    RuntimeExit(0);
}

struct Struct_1;

/** function System.CompareStr(a: System.String; b: System.String): Integer: (class 1, class 1) -> i32 **/
int32_t Function_10(struct Rc* L1, struct Rc* L2);

/** function System.Displayable.ToString(self: Boolean): System.String: (bool) -> class 1 **/
struct Rc* Function_11(bool L1);

/** <generated releaser for System::String>: (^{struct 1}) -> none **/
void Function_1(struct Struct_1* L0);

/** function System.Disposable.Dispose(self: System.String): Nothing: (class 1) -> none **/
void Function_7(struct Rc* L0);

/** function System.Displayable.ToString(self: Integer): System.String: (i32) -> class 1 **/
struct Rc* Function_3(int32_t L1);

/** function System.Comparable.Compare(self: System.String; other: System.String): Integer: (class 1, class 1) -> i32 **/
int32_t Function_9(struct Rc* L1, struct Rc* L2);

/** function System.Displayable.ToString(self: System.String): System.String: (class 1) -> class 1 **/
struct Rc* Function_6(struct Rc* L1);

/** function System.Comparable.Compare(self: Integer; other: Integer): Integer: (i32, i32) -> i32 **/
int32_t Function_5(int32_t L1, int32_t L2);

/** generated RC retain func for System::String: (^{struct 1}) -> none **/
void Function_0(struct Struct_1* L0);

void ModuleInit();

struct MethodTable_0 {
  struct MethodTable base;
  void(* method_0 )(struct Rc*);
};

/** Method Dispose of interface System::Disposable **/
void Method_0_0(struct Rc* L0) {
  struct MethodTable* table = L0->class->iface_methods;
  while (table) {
    if (table->iface == 0) {
      struct MethodTable_0* my_table = (struct MethodTable_0*) table;
      void(* method_ptr )(struct Rc*) = my_table->method_0;
      method_ptr(L0);
      return;
    } else {
      table = table->next;
    }
  }
  abort();
}



struct MethodTable_1 {
  struct MethodTable base;
  struct Rc*(* method_0 )(struct Rc*);
};

/** Method ToString of interface System::Displayable **/
struct Rc* Method_1_0(struct Rc* L1) {
  struct MethodTable* table = L1->class->iface_methods;
  while (table) {
    if (table->iface == 1) {
      struct MethodTable_1* my_table = (struct MethodTable_1*) table;
      struct Rc*(* method_ptr )(struct Rc*) = my_table->method_0;
      return method_ptr(L1);
    } else {
      table = table->next;
    }
  }
  abort();
}



struct MethodTable_2 {
  struct MethodTable base;
  int32_t(* method_0 )(struct Rc*, struct Rc*);
};

/** Method Compare of interface System::Comparable **/
int32_t Method_2_0(struct Rc* L1, struct Rc* L2) {
  struct MethodTable* table = L1->class->iface_methods;
  while (table) {
    if (table->iface == 2) {
      struct MethodTable_2* my_table = (struct MethodTable_2*) table;
      int32_t(* method_ptr )(struct Rc*, struct Rc*) = my_table->method_0;
      return method_ptr(L1, L2);
    } else {
      table = table->next;
    }
  }
  abort();
}



struct MethodTable_2 ImplTable_1_2;
struct MethodTable_0 ImplTable_1_0;
struct MethodTable_1 ImplTable_1_1;
struct Class Class_1;

struct MethodTable_2 ImplTable_1_2 = {
  .base = {
    .iface = 2,
    .next = &ImplTable_1_0.base,
  },
  .method_0 = &Function_9,
};

struct MethodTable_0 ImplTable_1_0 = {
  .base = {
    .iface = 0,
    .next = &ImplTable_1_1.base,
  },
  .method_0 = &Function_7,
};

struct MethodTable_1 ImplTable_1_1 = {
  .base = {
    .iface = 1,
    .next = NULL,
  },
  .method_0 = &Function_6,
};

struct Class Class_1 = {
  .size = sizeof(struct Struct_1),
  .disposer = &Function_7,
  .cleanup = (RcCleanupFunc) &Function_1,
  .iface_methods = (struct MethodTable*) &ImplTable_1_2,
};


static struct Struct_1 String_1 = {
  .field_0 = (unsigned char*) "two", 
  .field_1 = 3,
};
static struct Rc StringRc_1 = {
  .resource = &String_1,
  .class = &Class_1,
  .count = -1,
};
static struct Struct_1 String_4 = {
  .field_0 = (unsigned char*) "false", 
  .field_1 = 5,
};
static struct Rc StringRc_4 = {
  .resource = &String_4,
  .class = &Class_1,
  .count = -1,
};
static struct Struct_1 String_2 = {
  .field_0 = (unsigned char*) "something else", 
  .field_1 = 14,
};
static struct Rc StringRc_2 = {
  .resource = &String_2,
  .class = &Class_1,
  .count = -1,
};
static struct Struct_1 String_3 = {
  .field_0 = (unsigned char*) "true", 
  .field_1 = 4,
};
static struct Rc StringRc_3 = {
  .resource = &String_3,
  .class = &Class_1,
  .count = -1,
};
static struct Struct_1 String_0 = {
  .field_0 = (unsigned char*) "one", 
  .field_1 = 3,
};
static struct Rc StringRc_0 = {
  .resource = &String_0,
  .class = &Class_1,
  .count = -1,
};
/** function System.CompareStr(a: System.String; b: System.String): Integer: (class 1, class 1) -> i32 **/
int32_t Function_10(struct Rc* L1, struct Rc* L2) {
int32_t L0;
{
/* %0 = i32 (return slot) */
/* %1 = rc System::String */
/* %2 = rc System::String */
RcRetain(L1);
RcRetain(L2);
int32_t L3;
{
{
bool L4;
{
bool L5;
{
int32_t* L6;
{
struct Rc** L7;
(L7 = &(L1));
(L6 = &((((struct Struct_1*)((*(L7)))->resource))->field_1));
}
int32_t L7;
(L7 = 0);
(L5 = ((*(L6)) == L7));
}
bool L6;
{
int32_t* L7;
{
struct Rc** L8;
(L8 = &(L2));
(L7 = &((((struct Struct_1*)((*(L8)))->resource))->field_1));
}
int32_t L8;
(L8 = 0);
(L6 = ((*(L7)) == L8));
}
(L4 = (L5 && L6));
}
if (L4) {
goto J1;
}

goto J2;
J1:
{
}
{
{
{
{
int32_t L5;
(L5 = 0);
(L0 = L5);
RcRelease(L2);
RcRelease(L1);
goto J0;
}
}
}
}
goto J2;
J2:
{
}
}
int32_t L4;
{
int32_t L5;
(L5 = 0);
(L4 = L5);
}
int32_t L5;
{
int32_t L6;
(L6 = 0);
(L5 = L6);
}
int32_t L6;
{
int32_t L7;
(L7 = 0);
(L6 = L7);
}
{
bool L7;
J3:
{
}
bool L8;
(L8 = true);
(L7 = (!(L8)));
if (L7) {
goto J5;
}

{
{
{
bool L9;
{
bool L10;
{
int32_t* L11;
(L11 = &(L4));
int32_t* L12;
{
struct Rc** L13;
(L13 = &(L1));
(L12 = &((((struct Struct_1*)((*(L13)))->resource))->field_1));
}
bool L13;
(L13 = ((*(L11)) > (*(L12))));
bool L14;
(L14 = ((*(L11)) == (*(L12))));
bool L15;
(L15 = (L13 || L14));
(L10 = (!(L15)));
}
bool L11;
{
int32_t* L12;
(L12 = &(L5));
int32_t* L13;
{
struct Rc** L14;
(L14 = &(L2));
(L13 = &((((struct Struct_1*)((*(L14)))->resource))->field_1));
}
bool L14;
(L14 = ((*(L12)) > (*(L13))));
bool L15;
(L15 = ((*(L12)) == (*(L13))));
bool L16;
(L16 = (L14 || L15));
(L11 = (!(L16)));
}
(L9 = (L10 && L11));
}
if (L9) {
goto J6;
}

goto J8;
J6:
{
}
{
{
{
{
unsigned char L10;
{
unsigned char* L11;
{
unsigned char** L12;
{
struct Rc** L13;
(L13 = &(L1));
(L12 = &((((struct Struct_1*)((*(L13)))->resource))->field_0));
}
int32_t* L13;
(L13 = &(L4));
(L11 = ((*(L12)) + (*(L13))));
}
(L10 = (*(L11)));
}
unsigned char L11;
{
unsigned char* L12;
{
unsigned char** L13;
{
struct Rc** L14;
(L14 = &(L2));
(L13 = &((((struct Struct_1*)((*(L14)))->resource))->field_0));
}
int32_t* L14;
(L14 = &(L5));
(L12 = ((*(L13)) + (*(L14))));
}
(L11 = (*(L12)));
}
int32_t* L12;
(L12 = &(L6));
int32_t L13;
{
bool L14;
{
unsigned char* L15;
(L15 = &(L10));
unsigned char* L16;
(L16 = &(L11));
(L14 = ((*(L15)) > (*(L16))));
}
if (L14) {
goto J9;
}

goto J11;
J9:
{
}
{
int32_t L15;
(L15 = 1);
(L13 = L15);
}
goto J10;
J11:
{
}
{
int32_t L15;
{
bool L16;
{
unsigned char* L17;
(L17 = &(L11));
unsigned char* L18;
(L18 = &(L10));
(L16 = ((*(L17)) > (*(L18))));
}
if (L16) {
goto J12;
}

goto J14;
J12:
{
}
{
int32_t L17;
(L17 = 1);
int32_t L18;
(L18 = (0 - L17));
(L15 = L18);
}
goto J13;
J14:
{
}
{
int32_t L17;
(L17 = 0);
(L15 = L17);
}
J13:
{
}
}
(L13 = L15);
}
J10:
{
}
}
((*(L12)) = L13);
int32_t* L14;
(L14 = &(L4));
int32_t L15;
{
int32_t* L16;
(L16 = &(L4));
int32_t L17;
(L17 = 1);
(L15 = ((*(L16)) + L17));
}
((*(L14)) = L15);
int32_t* L16;
(L16 = &(L5));
int32_t L17;
{
int32_t* L18;
(L18 = &(L5));
int32_t L19;
(L19 = 1);
(L17 = ((*(L18)) + L19));
}
((*(L16)) = L17);
}
}
}
}
goto J7;
J8:
{
}
{
{
{
{
bool L10;
{
bool L11;
{
int32_t* L12;
(L12 = &(L4));
int32_t* L13;
{
struct Rc** L14;
(L14 = &(L1));
(L13 = &((((struct Struct_1*)((*(L14)))->resource))->field_1));
}
bool L14;
(L14 = ((*(L12)) > (*(L13))));
bool L15;
(L15 = ((*(L12)) == (*(L13))));
bool L16;
(L16 = (L14 || L15));
(L11 = (!(L16)));
}
bool L12;
{
int32_t* L13;
(L13 = &(L4));
int32_t* L14;
{
struct Rc** L15;
(L15 = &(L2));
(L14 = &((((struct Struct_1*)((*(L15)))->resource))->field_1));
}
bool L15;
(L15 = ((*(L13)) > (*(L14))));
bool L16;
(L16 = ((*(L13)) == (*(L14))));
(L12 = (L15 || L16));
}
(L10 = (L11 && L12));
}
if (L10) {
goto J15;
}

goto J17;
J15:
{
}
{
{
{
{
int32_t* L11;
(L11 = &(L6));
int32_t L12;
(L12 = 1);
((*(L11)) = L12);
}
}
}
}
goto J16;
J17:
{
}
{
{
{
{
bool L11;
{
bool L12;
{
int32_t* L13;
(L13 = &(L5));
int32_t* L14;
{
struct Rc** L15;
(L15 = &(L2));
(L14 = &((((struct Struct_1*)((*(L15)))->resource))->field_1));
}
bool L15;
(L15 = ((*(L13)) > (*(L14))));
bool L16;
(L16 = ((*(L13)) == (*(L14))));
bool L17;
(L17 = (L15 || L16));
(L12 = (!(L17)));
}
bool L13;
{
int32_t* L14;
(L14 = &(L5));
int32_t* L15;
{
struct Rc** L16;
(L16 = &(L1));
(L15 = &((((struct Struct_1*)((*(L16)))->resource))->field_1));
}
bool L16;
(L16 = ((*(L14)) > (*(L15))));
bool L17;
(L17 = ((*(L14)) == (*(L15))));
(L13 = (L16 || L17));
}
(L11 = (L12 && L13));
}
if (L11) {
goto J18;
}

goto J20;
J18:
{
}
{
{
{
{
int32_t* L12;
(L12 = &(L6));
int32_t L13;
(L13 = 1);
int32_t L14;
(L14 = (0 - L13));
((*(L12)) = L14);
}
}
}
}
goto J19;
J20:
{
}
{
{
{
{
goto J5;
}
}
}
}
J19:
{
}
}
}
}
}
J16:
{
}
}
}
}
}
J7:
{
}
}
{
bool L9;
{
int32_t* L10;
(L10 = &(L6));
int32_t L11;
(L11 = 0);
(L9 = ((*(L10)) == L11));
(L9 = (!(L9)));
}
if (L9) {
goto J21;
}

goto J22;
J21:
{
}
{
{
{
goto J5;
}
}
}
goto J22;
J22:
{
}
}
}
}
goto J3;
}
J5:
{
}
int32_t* L7;
(L7 = &(L6));
(L3 = (*(L7)));
}
(L0 = L3);
RcRelease(L2);
RcRelease(L1);
}
J0:
{
}
return L0;}

/** function System.Displayable.ToString(self: Boolean): System.String: (bool) -> class 1 **/
struct Rc* Function_11(bool L1) {
struct Rc* L0;
{
/* %0 = rc System::String (return slot) */
/* %1 = bool */
struct Rc* L2 = NULL;
{
struct Rc* L3 = NULL;
{
bool* L4;
(L4 = &(L1));
if ((*(L4))) {
goto J1;
}

goto J3;
J1:
{
}
{
struct Rc* L5 = NULL;
(L5 = &StringRc_3);
(L3 = L5);
RcRetain(L3);
}
goto J2;
J3:
{
}
{
struct Rc* L5 = NULL;
(L5 = &StringRc_4);
(L3 = L5);
RcRetain(L3);
}
J2:
{
}
}
(L2 = L3);
RcRetain(L3);
RcRelease(L3);
}
(L0 = L2);
RcRetain(L0);
RcRelease(L2);
}
return L0;}

/** <generated releaser for System::String>: (^{struct 1}) -> none **/
void Function_1(struct Struct_1* L0) {
{
int32_t* L1;
(L1 = &(((*(L0))).field_1));
unsigned char** L2;
(L2 = &(((*(L0))).field_0));
}
}

/** function System.Disposable.Dispose(self: System.String): Nothing: (class 1) -> none **/
void Function_7(struct Rc* L0) {
{
/* %0 = rc System::String */
RcRetain(L0);
{
{
bool L1;
{
int32_t* L2;
{
struct Rc** L3;
(L3 = &(L0));
(L2 = &((((struct Struct_1*)((*(L3)))->resource))->field_1));
}
int32_t L3;
(L3 = 0);
(L1 = ((*(L2)) > L3));
}
if (L1) {
goto J1;
}

goto J2;
J1:
{
}
{
{
{
{
unsigned char** L2;
{
struct Rc** L3;
(L3 = &(L0));
(L2 = &((((struct Struct_1*)((*(L3)))->resource))->field_0));
}
System_FreeMem((*(L2)));
}
}
}
}
goto J2;
J2:
{
}
}
unsigned char** L1;
{
struct Rc** L2;
(L2 = &(L0));
(L1 = &((((struct Struct_1*)((*(L2)))->resource))->field_0));
}
unsigned char* L2;
(L2 = NULL);
((*(L1)) = L2);
int32_t* L3;
{
struct Rc** L4;
(L4 = &(L0));
(L3 = &((((struct Struct_1*)((*(L4)))->resource))->field_1));
}
int32_t L4;
(L4 = 0);
((*(L3)) = L4);
}
RcRelease(L0);
}
}

/** function System.Displayable.ToString(self: Integer): System.String: (i32) -> class 1 **/
struct Rc* Function_3(int32_t L1) {
struct Rc* L0;
{
/* %0 = rc System::String (return slot) */
/* %1 = i32 */
struct Rc* L2 = NULL;
{
struct Rc* L3 = NULL;
{
int32_t* L4;
(L4 = &(L1));
(L3 = System_IntToStr((*(L4))));
}
(L2 = L3);
RcRetain(L3);
RcRelease(L3);
}
(L0 = L2);
RcRetain(L0);
RcRelease(L2);
}
return L0;}

/** function System.Comparable.Compare(self: System.String; other: System.String): Integer: (class 1, class 1) -> i32 **/
int32_t Function_9(struct Rc* L1, struct Rc* L2) {
int32_t L0;
{
/* %0 = i32 (return slot) */
/* %1 = rc System::String */
/* %2 = rc System::String */
RcRetain(L1);
RcRetain(L2);
int32_t L3;
{
int32_t L4;
{
struct Rc** L5;
(L5 = &(L1));
struct Rc** L6;
(L6 = &(L2));
(L4 = Function_10((*(L5)), (*(L6))));
}
(L3 = L4);
}
(L0 = L3);
RcRelease(L2);
RcRelease(L1);
}
return L0;}

/** function System.Displayable.ToString(self: System.String): System.String: (class 1) -> class 1 **/
struct Rc* Function_6(struct Rc* L1) {
struct Rc* L0;
{
/* %0 = rc System::String (return slot) */
/* %1 = rc System::String */
RcRetain(L1);
struct Rc* L2 = NULL;
{
struct Rc** L3;
(L3 = &(L1));
(L2 = (*(L3)));
RcRetain((*(L3)));
}
(L0 = L2);
RcRetain(L0);
RcRelease(L2);
RcRelease(L1);
}
return L0;}

/** function System.Comparable.Compare(self: Integer; other: Integer): Integer: (i32, i32) -> i32 **/
int32_t Function_5(int32_t L1, int32_t L2) {
int32_t L0;
{
/* %0 = i32 (return slot) */
/* %1 = i32 */
/* %2 = i32 */
int32_t L3;
{
int32_t L4;
{
int32_t* L5;
(L5 = &(L1));
int32_t* L6;
(L6 = &(L2));
(L4 = ((*(L5)) - (*(L6))));
}
(L3 = L4);
}
(L0 = L3);
}
return L0;}

/** generated RC retain func for System::String: (^{struct 1}) -> none **/
void Function_0(struct Struct_1* L0) {
{
int32_t* L1;
(L1 = &(((*(L0))).field_1));
unsigned char** L2;
(L2 = &(((*(L0))).field_0));
}
}

void ModuleInit() {
{
struct Rc* L0 = NULL;
{
struct Rc* L1 = NULL;
{
int32_t L2;
(L2 = 1);
{
int32_t L3;
(L3 = 1);
bool L4;
(L4 = (L3 == L2));
if (L4) {
goto J2;
}

}
{
int32_t L3;
(L3 = 2);
bool L4;
(L4 = (L3 == L2));
if (L4) {
goto J3;
}

}
goto J4;
J2:
{
}
struct Rc* L3 = NULL;
(L3 = &StringRc_0);
(L3 = L1);
goto J1;
J3:
{
}
struct Rc* L4 = NULL;
(L4 = &StringRc_1);
(L4 = L1);
goto J1;
J4:
{
}
struct Rc* L5 = NULL;
(L5 = &StringRc_2);
(L5 = L1);
goto J1;
J1:
{
}
}
(L0 = L1);
RcRetain(L0);
}
{
struct Rc** L1;
(L1 = &(L0));
System_WriteLn((*(L1)));
}
RcRelease(L0);
}
}

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

