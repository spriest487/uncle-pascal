#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
typedef int8_t System_Byte;
typedef int64_t System_Integer;
typedef const char* System_String;
typedef void* System_Pointer;
typedef bool System_Boolean;
static void System_WriteLn(System_String ln) {
    if (!ln) abort();

    puts(ln);
}

static System_Byte* System_GetMem(System_Integer bytes) {
    if (bytes > 0) {
        return malloc((size_t) bytes);
    } else {
        return NULL;
    }
}

static void System_FreeMem(System_Byte* p) {
    free(p);
}
/* Vector interface */
struct Vector_Vector {
System_Byte* Elements;
System_Integer Length;
};

struct Vector_Vector Vector_Create () {
struct Vector_Vector result;
memset(&result, 0, sizeof(result));
return result;
{
(result.Elements = ((System_Integer) 0));
(result.Length = ((System_Integer) 0));
}
}

void Vector_Add (struct Vector_Vector* self, System_Byte p) {
{
System_Byte* newElements =System_GetMem(((*self).Length + ((System_Integer) 1)));
if (((*self).Elements != ((System_Integer) 0))) {
for (System_Integer i =((System_Integer) 0); i < (*self).Length; i += 1) {
((*(newElements + i)) = (*((*self).Elements + i)));}
;
System_FreeMem((*self).Elements);
}
;
((*self).Length = ((*self).Length + ((System_Integer) 1)));
((*self).Elements = newElements);
((*((*self).Elements + ((*self).Length - ((System_Integer) 1)))) = p);
}
}

void Vector_AddAll (struct Vector_Vector* self, struct Vector_Vector* other) {
{
for (System_Integer i =((System_Integer) 0); i < (*other).Length; i += 1) {
Vector_Add(self, (*((*other).Elements + i)));}
;
}
}

/* Vector implementation */
/* program decls */
System_String Greet (System_String name) {
System_String result;
result = "";
return result;
{
System_String msg =((System_String)"hello world");
(result = msg);
}
}

struct Vector_Vector vec;
/* program vars */
/* program main */
int main(int argc, char* argv[]) {
memset(&vec, 0, sizeof(vec));
{
System_Integer x =((System_Integer) 1);
System_Integer y =((System_Integer) 2);
(x = (x + y));
(vec = Vector_Create());
Vector_Add((&vec), ((System_Integer) 1));
Vector_Add((&vec), ((System_Integer) 2));
Vector_Add((&vec), ((System_Integer) 3));
Vector_Add((&vec), ((System_Integer) 4));
System_WriteLn(((System_String)"hello world"));
if ((x == ((System_Integer) 1))) {
System_WriteLn(((System_String)"one"));
}
 else {
System_WriteLn(((System_String)"not one"));
}
;
}
  return 0;
}
