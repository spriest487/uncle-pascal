//#define UNCLEPASCAL_RC_DEBUG
//#define UNCLEPASCAL_REFLECTION_DEBUG

#include <sstream>
#include <iostream>

struct System_Internal_Class {
    std::string Name;
    System_Internal_Destructor Destructor;
};

static void System_Internal_Raise(const char* file, int line, int col, const char* msg) {
    std::fprintf(stderr, "Error raised in %s @ %d:%d: %s\n", file, line, col, msg);
    std::exit(1);
}

static System_Byte* System_GetMem(System_NativeInt bytes) {
    if (bytes > 0) {
        auto mem = static_cast<System_Byte*>(std::malloc(static_cast<std::size_t>(bytes)));
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
    std::free(p);
}

static std::unordered_map<std::string, std::unique_ptr<System_Internal_Class>> System_Internal_Classes;

static void System_Internal_InitClass(const char* name,
        System_Internal_Destructor destructor) {
    auto nameStr = std::string(name);
    if (System_Internal_Classes.find(nameStr) != System_Internal_Classes.end()) {
        std::fprintf(stderr, "attempting to initialize class with duplicate name");
        abort();
    }

    auto classObj = std::make_unique<System_Internal_Class>();
    classObj->Name = nameStr;
    classObj->Destructor = destructor;

    System_Internal_Classes.insert(make_pair(nameStr, move(classObj)));

#ifdef UNCLEPASCAL_REFLECTION_DEBUG
    std::fprintf(stderr, "initialized class %s\n", name);
#endif
}

static System_Internal_Class* System_Internal_FindClass(const char* name) {
    auto classIt = System_Internal_Classes.find(name);
    if (classIt != System_Internal_Classes.end()) {
        return classIt->second.get();
    }
    return nullptr;
}

static System_Internal_Object* System_Internal_Rc_GetMem(System_NativeInt size, const char* constructorName) {
    auto obj = reinterpret_cast<System_Internal_Object*>(System_GetMem(size));

    /* class-type objects are zero-initialized*/
    std::memset(obj, 0, static_cast<std::size_t>(size));

    obj->Class = System_Internal_FindClass(constructorName);
    if (!obj->Class) {
        std::fprintf(stderr, "missing class definition for %s\n", constructorName);
        std::abort();
    }

    obj->StrongCount = 1;

#ifdef UNCLEPASCAL_RC_DEBUG
    std::fprintf(stderr, "rc allocated %td bytes for %s @ %p\n", size, constructorName, obj);
#endif

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
        if (!obj->Class) {
            std::fprintf(stderr, "missing class reference for object @ %p\n", obj);
            std::abort();
        }

        auto& className = obj->Class->Name;
        if (obj->Class->Destructor) {
            obj->Class->Destructor(obj);
        }

        std::free(obj);

#ifdef UNCLEPASCAL_RC_DEBUG
        fprintf(stderr, "rc deallocated %s @ %p\n", className.c_str(), obj);
#endif
    }
}

/* procedure System.WriteLn(line: System.String) */
void System_WriteLn(System_String* lineRc) {
    auto line = static_cast<System_String*>(lineRc);

    if (line) {
        for (int c = 0; c < line->Length; ++c) {
            fputc(line->Chars[c], stdout);
        }
        fputc('\n', stdout);
    } else {
        puts("");
    }
}

System_String* System_ReadLn() {
    std::string line;
    if (std::cin >> line) {
        return System_StringFromBytes(
            (System_Byte*)line.data(), 
            (System_NativeInt)line.size());
    } else {
        // failed, empty string
        return System_StringCreate();
    }
}

System_Boolean System_StringToInt(System_String* str, System_Int32* result) {
    auto stream = std::stringstream();
    stream.str(std::string(
        reinterpret_cast<const char*>(str->Chars),
        static_cast<std::size_t>(str->Length)));

    if (stream >> *result) {
        return true;
    } else {
        return false;
    }
}

System_String* System_StringFromInt(System_Int32 i) {
    auto chars = std::to_string(i);

    return System_StringFromBytes(
        reinterpret_cast<System_Byte*>(const_cast<char*>(chars.data())),
        static_cast<System_NativeUInt>(chars.size()));
}
