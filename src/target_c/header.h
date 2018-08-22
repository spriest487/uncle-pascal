#define _CRT_SECURE_NO_DEPRECATE

#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include <unordered_map>
#include <unordered_set>
#include <string>
#include <utility>
#include <memory>

#ifdef __APPLE__
#include <experimental/optional>

template<typename T>
using System_Internal_Option = std::experimental::optional<T>;
#define System_Internal_None std::experimental::nullopt
#else
#include <optional>

template<typename T>
using System_Internal_Option = std::optional<T>;
#define System_Internal_None std::nullopt
#endif

#define System_Internal_SizeOf(x) (sizeof(x))

typedef std::uint8_t PascalType_System_Byte;
typedef std::int32_t PascalType_System_Int32;
typedef std::uint32_t PascalType_System_UInt32;
typedef std::int64_t PascalType_System_Int64;
typedef std::uint64_t PascalType_System_UInt64;
typedef std::ptrdiff_t PascalType_System_NativeInt;
typedef std::size_t PascalType_System_NativeUInt;
typedef double PascalType_System_Float64;

typedef void* PascalType_System_Pointer;
typedef std::uint8_t PascalType_System_Boolean;

/* intialized before class init with the ID of `System.Disposable` */
static PascalType_System_NativeUInt System_Internal_DisposeInterfaceID;

template<typename T>
using System_Internal_Set = std::unordered_set<T>;

template<typename E>
struct System_Internal_Array {
    E member_Elements;
};

#ifdef _WIN32
template<typename R, typename... Args>
using System_Internal_Func_Stdcall = R (__stdcall *)(Args...);
#else
template<typename R, typename... Args>
using System_Internal_Func_Stdcall = R (*)(Args...);
#endif

template<typename R, typename... Args>
using System_Internal_Func_Cdecl = R (__cdecl *)(Args...);

struct System_Internal_Class;

typedef void(*System_Internal_Destructor)(const void* instance);

struct System_Internal_Object {
    System_Internal_Class* Class;
    PascalType_System_NativeUInt StrongCount;
};

struct System_Internal_InterfaceImpl {
    PascalType_System_NativeUInt ID;
    const void* VTable;
};

static void System_Internal_InitClass(const char* name,
    System_Internal_InterfaceImpl* interfaces,
    PascalType_System_NativeUInt interfaceCount,
    System_Internal_Destructor destructor);

static System_Internal_Class* System_Internal_FindClass(const char* name);
static const void* System_Internal_FindVTable(System_Internal_Object* obj,
    PascalType_System_NativeUInt interfaceID);

static void System_Internal_Rc_Retain(System_Internal_Object* obj);
static void System_Internal_Rc_Release(System_Internal_Object* obj);
static System_Internal_Object* System_Internal_Rc_GetMem(
    PascalType_System_NativeUInt size,
    const char* constructorName);

static void System_Internal_Raise(const char* file, int line, int col, const char* msg);

static void System_Internal_ZeroMemory(void* mem, PascalType_System_NativeUInt len);
