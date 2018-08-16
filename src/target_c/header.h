#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include <unordered_map>
#include <string>
#include <utility>
#include <memory>

typedef std::int8_t System_Byte;
typedef std::int32_t System_Int32;
typedef std::uint32_t System_UInt32;
typedef std::int64_t System_Int64;
typedef std::uint64_t System_UInt64;
typedef std::ptrdiff_t System_NativeInt;
typedef std::size_t System_NativeUInt;

typedef void* System_Pointer;
typedef std::uint8_t System_Boolean;

template<typename E>
struct System_Internal_Array {
    E Elements;
};

struct System_Internal_Class;

struct System_Internal_Object {
    System_Internal_Class* Class;
    System_NativeUInt StrongCount;
};

typedef void (*System_Internal_Destructor)(System_Internal_Object*);

static System_Byte* System_GetMem(System_NativeInt bytes);
static void System_FreeMem(System_Byte* p);

static void System_Internal_InitClass(const char* name, System_Internal_Destructor destructor);
static System_Internal_Class* System_Internal_FindClass(const char* name);

static System_Internal_Object* System_Internal_Rc_GetMem(System_NativeInt size, const char* constructorName);
static void System_Internal_Rc_Retain(System_Internal_Object* obj);
static void System_Internal_Rc_Release(System_Internal_Object* obj);

struct System_String;

/* procedure System.WriteLn(line: System.String) */
static void System_WriteLn(System_String* lineRc);
