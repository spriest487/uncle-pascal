#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include <unordered_map>
#include <string>
#include <utility>
#include <memory>

typedef std::int8_t System_Byte;
typedef std::int64_t System_Integer;
typedef void* System_Pointer;
typedef std::uint8_t System_Boolean;

struct System_Internal_Class;

struct System_Internal_Object {
    System_Internal_Class* Class;
    System_Integer StrongCount;
};

typedef void (*System_Internal_Destructor)(System_Internal_Object*);

static System_Byte* System_GetMem(System_Integer bytes);
static void System_FreeMem(System_Byte* p);

static void System_Internal_InitClass(const char* name, System_Internal_Destructor destructor);
static System_Internal_Class* System_Internal_FindClass(const char* name);

static System_Internal_Object* System_Internal_Rc_GetMem(System_Integer size, const char* constructorName);
static void System_Internal_Rc_Retain(System_Internal_Object* obj);
static void System_Internal_Rc_Release(System_Internal_Object* obj);

/* strings have special support in the compiler (e.g. concatenation sugar)
so we need to declare them here instead of in a Pascal file */
struct System_String : System_Internal_Object {
    System_Byte* Chars;
    System_Integer Length;
};

/* procedure System.WriteLn(line: System.String) */
static void System_WriteLn(System_String* lineRc);
