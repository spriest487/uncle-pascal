//#define UNCLEPASCAL_RC_DEBUG
//#define UNCLEPASCAL_REFLECTION_DEBUG

#include <sstream>
#include <iostream>

struct System_Internal_Class {
    std::string Name;

    System_Internal_InterfaceImpl* Interfaces;
    PascalType_System_NativeUInt InterfaceCount;

    System_Internal_Destructor Destructor;
};

static const void* System_Internal_TryFindVTable(
        System_Internal_Object* obj,
        PascalType_System_NativeUInt interfaceID) {
    struct System_Internal_Class* objClass = obj->Class;
    for (PascalType_System_NativeUInt i = 0;
            i < objClass->InterfaceCount;
            ++i) {
        struct System_Internal_InterfaceImpl* iface = &objClass->Interfaces[i];

        if (iface->ID == interfaceID) {
            return iface->VTable;
        }
    }

    return nullptr;
}

static const void* System_Internal_FindVTable(
        System_Internal_Object* obj,
        PascalType_System_NativeUInt interfaceID) {
    const void* vtable = System_Internal_TryFindVTable(obj, interfaceID);

    if (!vtable) {
        std::fprintf(stderr, "missing vtable of interface $%td for class %s\n",
            interfaceID,
            obj->Class->Name.c_str());
        std::abort();
    }

    return vtable;
}

static void System_Internal_Raise(const char* file, int line, int col, const char* msg) {
    std::fprintf(stderr, "Error raised in %s @ %d:%d: %s\n", file, line, col, msg);
    std::exit(1);
}

static void System_Internal_ZeroMemory(void* mem, PascalType_System_NativeUInt len) {
    std::memset(mem, 0, len);
}

static PascalType_System_Byte* Pascal_System_GetMem(PascalType_System_NativeUInt bytes) {
    if (bytes > 0) {
        auto mem = static_cast<PascalType_System_Byte*>(std::malloc(static_cast<std::size_t>(bytes)));
        if (!mem) {
            fputs("memory allocation failed\n", stderr);
            abort();
        }
        return mem;
    } else {
        return nullptr;
    }
}

static void Pascal_System_FreeMem(PascalType_System_Byte* p) {
    std::free(p);
}

static std::unordered_map<std::string, std::unique_ptr<System_Internal_Class>> System_Internal_Classes;

static void System_Internal_InitClass(const char* name,
        System_Internal_InterfaceImpl* interfaces,
        PascalType_System_NativeUInt interfaceCount,
        System_Internal_Destructor destructor) {
    auto nameStr = std::string(name);
    if (System_Internal_Classes.find(nameStr) != System_Internal_Classes.end()) {
        std::fprintf(stderr, "attempting to initialize class with duplicate name");
        abort();
    }

    auto classObj = std::make_unique<System_Internal_Class>();
    classObj->Name = nameStr;

    classObj->Interfaces = interfaces,
    classObj->InterfaceCount = interfaceCount;
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

static System_Internal_Object* System_Internal_Rc_GetMem(
        PascalType_System_NativeUInt size,
        const char* constructorName) {
    auto obj = reinterpret_cast<System_Internal_Object*>(Pascal_System_GetMem(size));

    /* class-type objects are zero-initialized*/
    System_Internal_ZeroMemory(obj, static_cast<std::size_t>(size));

    obj->Class = System_Internal_FindClass(constructorName);
    if (!obj->Class) {
        std::fprintf(stderr, "missing class definition for %s\n", constructorName);
        std::abort();
    }

    obj->StrongCount = 1;
    obj->WeakCount = 0;

#ifdef UNCLEPASCAL_RC_DEBUG
    std::fprintf(stderr, "rc allocated %td bytes for %s @ %p\n", size, constructorName, obj);
#endif

    return obj;
}

static void System_Internal_Rc_Retain(System_Internal_Object* obj) {
#ifdef UNCLEPASCAL_RC_DEBUG
    if (!obj) {
        std::fprintf(stderr, "retained invalid rc @ %p\n", obj);
        std::abort();
    }
#endif

    obj->StrongCount += 1;
}

static void System_Internal_Rc_RetainWeak(System_Internal_Object* obj) {
    obj->WeakCount += 1;
}

static void System_Internal_Rc_ReleaseWeak(System_Internal_Object* obj) {
    if (obj-> WeakCount == 1 && obj->StrongCount == 0) {
        /* this is the last weak ref to a dead object, release the instance memory */
        std::free(obj);
    } else {
        obj->WeakCount -= 1;
    }
}

static void System_Internal_Rc_Release(System_Internal_Object* obj) {
#ifdef UNCLEPASCAL_RC_DEBUG
    if (!obj) {
        std::fprintf(stderr, "released NULL rc\n");
        std::abort();
    }
#endif

    if (obj->StrongCount > 1) {
        /* object is still alive, reduce its refcount */
        obj->StrongCount -= 1;
    } else {
        /* releasing the last ref to an object, destroy it now */
#ifdef UNCLEPASCAL_RC_DEBUG
        if (!obj->Class) {
            std::fprintf(stderr, "missing class reference for object @ %p\n", obj);
            std::abort();
        }
#endif

        /* we need to call System.Disposable.Dispose() if this class implements it */
        auto disposeImpl = static_cast<const PascalType_System_Disposable_VTable*>(
            System_Internal_TryFindVTable(obj, System_Internal_DisposeInterfaceID)
        );

        if (disposeImpl) {
            disposeImpl->member_Dispose(obj);

            /* increasing the refcount during destruction is fine, but if it's
            not 1 by the time the destructor exits, it will leak */
            if (obj->StrongCount > 1) {
                std::fprintf(stderr, "Dispose() increased refcount for object @ %p\n", obj);
                std::abort();
            }
        }

        /* actually destroy the object. from this point onwards the instance is
        logically dead, even if the memory isn't freed yet */
        obj->Class->Destructor(obj);

        /* if there's still weak refs active, don't free the memory yet */
        if (obj->WeakCount == 0) {
            std::free(obj);
        } else {
            obj->StrongCount = 0;
        }

#ifdef UNCLEPASCAL_RC_DEBUG
        auto& className = obj->Class->Name;
        fprintf(stderr, "rc deallocated %s @ %p\n", className.c_str(), obj);
#endif
    }
}

/* procedure System.WriteLn(line: System.String) */
void Pascal_System_WriteLn(PascalType_System_String* lineRc) {
    struct PascalType_System_String* line = static_cast<struct PascalType_System_String*>(lineRc);

    if (line) {
        for (PascalType_System_NativeUInt c = 0;
                c < line->member_Length;
                ++c) {
            fputc(line->member_Chars[c], stdout);
        }
        fputc('\n', stdout);
    } else {
        puts("");
    }
}

struct PascalType_System_String* Pascal_System_ReadLn() {
    std::string line;
    if (std::cin >> line) {
        return Pascal_System_StringFromBytes(
            (PascalType_System_Byte*)line.data(),
            (PascalType_System_NativeInt)line.size());
    } else {
        // failed, empty string
        return Pascal_System_StringCreate();
    }
}

PascalType_System_Boolean Pascal_System_StringToInt(
        PascalType_System_String* str,
        PascalType_System_Int32& result) {
    auto stream = std::stringstream();
    stream.str(std::string(
        reinterpret_cast<const char*>(str->member_Chars),
        static_cast<std::size_t>(str->member_Length)));

    if (stream >> result) {
        return true;
    } else {
        return false;
    }
}

PascalType_System_String* Pascal_System_StringFromInt(
        PascalType_System_Int32 i) {
    auto chars = std::to_string(i);

    return Pascal_System_StringFromBytes(
        reinterpret_cast<PascalType_System_Byte*>(const_cast<char*>(chars.data())),
        static_cast<PascalType_System_NativeUInt>(chars.size()));
}

PascalType_System_Pointer Pascal_IO_FOpen(
    PascalType_System_Byte* fileNameStr,
    PascalType_System_NativeInt mode
) {
    const char* modeStr;
    switch (mode) {
        case 1: modeStr = "wb"; break;
        default: modeStr = "rb"; break;
    }

    const char* fileNameChars = reinterpret_cast<const char*>(fileNameStr);

    FILE* file = std::fopen(fileNameChars, modeStr);
    return static_cast<PascalType_System_Pointer>(file);
}

void Pascal_IO_FClose(PascalType_System_Pointer fileHandle) {
    FILE* file = static_cast<FILE*>(fileHandle);
    if (std::fclose(file) != 0) {
        std::fprintf(stderr, "tried to close a file that wasn't open\n");
        std::abort();
    }
}

PascalType_System_NativeUInt Pascal_IO_FRead(
        PascalType_System_Pointer fileHandle,
        PascalType_System_Byte* to,
        PascalType_System_NativeUInt len) {
    FILE* file = static_cast<FILE*>(fileHandle);
    std::size_t readCount = std::fread(to, 1, len, file);
    
    return static_cast<PascalType_System_NativeUInt>(readCount);
}

PascalType_System_Boolean Pascal_IO_FEof(PascalType_System_Pointer fileHandle) {
    FILE* file = static_cast<FILE*>(fileHandle);
    return std::feof(file);
}
