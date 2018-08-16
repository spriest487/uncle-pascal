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
};

struct System_String : System_Internal_Object {
    System_Byte* Chars;
    System_Integer Length;
};

struct System_Internal_RefCount {
    System_Integer StrongCount;
};

struct System_Internal_Rc {
    System_Pointer Value;
    System_Internal_RefCount* RefCount;
};

static std::unordered_map<std::string, std::unique_ptr<System_Internal_Class>> System_Internal_Classes;

static System_Internal_Rc System_StringFromBytes(System_Byte* bytes, System_Integer len);

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

static System_Internal_Rc System_Internal_Rc_GetMem(System_Integer size, const char* constructorName) {
    System_Internal_Rc rc;
    rc.Value = static_cast<System_Byte*>(malloc(static_cast<size_t>(size)));
    if (!rc.Value) {
        std::fprintf(stderr, "object memory allocation failed in %s constructor\n", constructorName);
        std::abort();
    }

    auto asObj = reinterpret_cast<System_Internal_Object*>(rc.Value);
    asObj->Class = System_Internal_FindClass(constructorName);

    rc.RefCount = static_cast<System_Internal_RefCount*>(malloc(sizeof(System_Internal_RefCount)));
    if (!rc.RefCount) {
        std::fprintf(stderr, "rc memory allocation failed in %s constructor\n", constructorName);
        std::abort();
    }

    rc.RefCount->StrongCount = 0;

    std::fprintf(stderr, "rc allocated %lld bytes for %s @ %p + %p\n", size, constructorName, rc.RefCount, rc.Value);
    return rc;
}

static void System_Internal_Rc_Retain(System_Internal_Rc* rc) {
    if (!rc->RefCount) {
        std::fprintf(stderr, "retained rc that was already deallocated @ %p + %p\n", rc->RefCount, rc->Value);
        std::abort();
    }

    rc->RefCount->StrongCount += 1;
}

static void System_Internal_Rc_Release(System_Internal_Rc* rc) {
    if (!rc->RefCount || rc->RefCount->StrongCount <= 0) {
        std::fprintf(stderr, "released rc that was already released @ %p + %p\n", rc->RefCount, rc->Value);
        std::abort();
    }

    rc->RefCount->StrongCount -= 1;
    if (rc->RefCount->StrongCount == 0) {
        auto instance = static_cast<System_Internal_Object*>(rc->Value);
        auto& className = instance->Class->Name;

        fprintf(stderr, "rc deallocated %s @ %p + %p\n", className.c_str(), rc->RefCount, rc->Value);

        std::free(rc->Value);
        std::free(rc->RefCount);
        rc->RefCount = nullptr;
        rc->Value = nullptr;
    }
}

/* procedure System.WriteLn(line: System.String) */
static void System_WriteLn(System_Internal_Rc lineRc) {
    System_Internal_Rc_Retain(&lineRc);

    auto line = static_cast<System_String*>(lineRc.Value);

    if (line) {
        for (int c = 0; c < line->Length; ++c) {
            fputc(line->Chars[c], stdout);
        }
        fputc('\n', stdout);
    } else {
        puts("");
    }

    System_Internal_Rc_Release(&lineRc);
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
static System_Internal_Rc System_StringFromBytes(System_Byte* bytes, System_Integer len) {
    auto string = static_cast<System_Byte*>(malloc((size_t)len));
    if (!string) {
        fputs("string allocation failed in System_StringFromBytes\n", stderr);
        abort();
    }

    memcpy(string, bytes, (size_t)len);

    auto result = System_Internal_Rc_GetMem(sizeof(System_String), "System.String");
    auto resultStr = static_cast<System_String*>(result.Value);
    resultStr->Chars = string;
    resultStr->Length = len;

    return result;
}

/* function System.StringFromBytes(a: System.String; b: System.String): System.String */
static System_Internal_Rc System_StringConcat(System_Internal_Rc aRc, System_Internal_Rc bRc) {
    System_Internal_Rc_Retain(&aRc);
    System_Internal_Rc_Retain(&bRc);

    auto a = static_cast<System_String*>(aRc.Value);
    auto b = static_cast<System_String*>(bRc.Value);

    auto emptyA = !a || !a->Length;
    auto emptyB = !b || !b->Length;

    System_Internal_Rc result;
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

    System_Internal_Rc_Release(&aRc);
    System_Internal_Rc_Release(&bRc);

    return result;
}