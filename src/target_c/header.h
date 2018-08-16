#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include <unordered_map>
#include <string>
#include <string_view>
#include <utility>
#include <memory>

typedef std::int8_t System_Byte;
typedef std::int64_t System_Integer;
typedef void* System_Pointer;
typedef std::uint8_t System_Boolean;

struct System_Internal_Class {
    std::string Name;
};

struct System_Internal_Object {
    System_Internal_Class* Class;
    System_Integer StrongCount;
};

struct System_String : System_Internal_Object {
    System_Byte* Chars;
    System_Integer Length;
};

static std::unordered_map<std::string, std::unique_ptr<System_Internal_Class>> System_Internal_Classes;

static System_String* System_StringFromBytes(System_Byte* bytes, System_Integer len);

static void System_Internal_InitClass(const char* name) {
    static bool internalClassInit = false;
    if (!internalClassInit) {
        // init classes required to init other classes
        auto stringName = std::string("System.String");
        auto stringClass = std::make_unique<System_Internal_Class>();
        stringClass->Name = stringName;

        System_Internal_Classes.insert(make_pair(stringName, move(stringClass)));

        internalClassInit = true;
    }

    auto nameStr = std::string(name);
    if (System_Internal_Classes.find(nameStr) != System_Internal_Classes.end()) {
        std::fprintf(stderr, "attempting to initialize class with duplicate name");
        abort();
    }

    auto classObj = std::make_unique<System_Internal_Class>();
    classObj->Name = nameStr;

    System_Internal_Classes.insert(make_pair(nameStr, move(classObj)));

    std::fprintf(stderr, "initialized class %s\n", name);
}

static System_Internal_Class* System_Internal_FindClass(const char* name) {
    auto classIt = System_Internal_Classes.find(name);
    if (classIt != System_Internal_Classes.end()) {
        return classIt->second.get();
    }
    return nullptr;
}

static System_Internal_Object* System_Internal_Rc_GetMem(System_Integer size, const char* constructorName) {
    auto obj = static_cast<System_Internal_Object*>(std::malloc(static_cast<std::size_t>(size)));
    obj->Class = System_Internal_FindClass(constructorName);
    obj->StrongCount = 0;

    std::fprintf(stderr, "rc allocated %lld bytes for %s @ %p\n", size, constructorName, obj);
    return obj;
}

static void System_Internal_Rc_Retain(System_Internal_Object* obj) {
    if (!obj) {
        std::fprintf(stderr, "retained invalid rc @ %p\n", obj);
        std::abort();
    }

    obj->StrongCount += 1;
}

static void System_Internal_Rc_Release(System_Internal_Object* obj) {
    if (!obj || obj->StrongCount <= 0) {
        std::fprintf(stderr, "released rc that was already released @ %p\n", obj);
        std::abort();
    }

    obj->StrongCount -= 1;
    if (obj->StrongCount == 0) {
        auto& className = obj->Class->Name;

        fprintf(stderr, "rc deallocated %s @ %p\n", className.c_str(), obj);

        std::free(obj);
    }
}

/* procedure System.WriteLn(line: System.String) */
static void System_WriteLn(System_String* lineRc) {
    System_Internal_Rc_Retain(lineRc);

    auto line = static_cast<System_String*>(lineRc);

    if (line) {
        for (int c = 0; c < line->Length; ++c) {
            fputc(line->Chars[c], stdout);
        }
        fputc('\n', stdout);
    } else {
        puts("");
    }

    System_Internal_Rc_Release(lineRc);
}

static System_Byte* System_GetMem(System_Integer bytes) {
    if (bytes > 0) {
        auto mem = static_cast<System_Byte*>(malloc(static_cast<size_t>(bytes)));
        if (!mem) {
            fputs("memory allocation failed\n", stderr);
            abort();
        }
        return mem;
    } else {
        return nullptr;
    }
}

static void System_FreeMem(System_Byte* p) {
    free(p);
}

/* function System.StringFromBytes(chars: ^System.Byte; len: System.Integer): System.String */
static System_String* System_StringFromBytes(System_Byte* bytes, System_Integer len) {
    auto string = static_cast<System_Byte*>(malloc((size_t)len));
    if (!string) {
        fputs("string allocation failed in System.StringFromBytes\n", stderr);
        abort();
    }

    std::memcpy(string, bytes, (size_t)len);

    auto result = System_Internal_Rc_GetMem(sizeof(System_String), "System.String");
    auto resultStr = static_cast<System_String*>(result);
    resultStr->Chars = string;
    resultStr->Length = len;

    return resultStr;
}

/* function System.StringFromBytes(a: System.String; b: System.String): System.String */
static System_String* System_StringConcat(System_Internal_Object* aObj, System_Internal_Object* bObj) {
    System_Internal_Rc_Retain(aObj);
    System_Internal_Rc_Retain(bObj);

    auto a = static_cast<System_String*>(aObj);
    auto b = static_cast<System_String*>(bObj);

    auto emptyA = !a || !a->Length;
    auto emptyB = !b || !b->Length;

    System_String* result;
    if (emptyA && emptyB) {
        result = System_StringFromBytes((System_Byte*)"", 0);
    } else if (emptyA) {
        result = System_StringFromBytes(b->Chars, b->Length);
    } else if (emptyB) {
        result = System_StringFromBytes(a->Chars, a->Length);
    } else {
        auto totalLength = a->Length + b->Length;
        auto chars = static_cast<System_Byte*>(malloc((size_t)totalLength));
        if (!chars) {
            fputs("string allocation failed in System_StringConcat\n", stderr);
            abort();
        }

        for (System_Integer c = 0; c < a->Length; ++c) {
            chars[c] = a->Chars[c];
        }

        for (System_Integer c = 0; c < b->Length; ++c) {
            chars[c + a->Length] = b->Chars[c];
        }

        result = System_StringFromBytes(chars, totalLength);
        free(chars);
    }

    System_Internal_Rc_Release(aObj);
    System_Internal_Rc_Release(bObj);

    return result;
}