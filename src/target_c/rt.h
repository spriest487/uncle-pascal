struct System_Internal_Class {
    std::string Name;
    System_Internal_Destructor Destructor;
};

static System_Byte* System_GetMem(System_Integer bytes) {
    if (bytes > 0) {
        auto mem = static_cast<System_Byte*>(std::malloc(static_cast<size_t>(bytes)));
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
    auto obj = reinterpret_cast<System_Internal_Object*>(System_GetMem(size));
    obj->Class = System_Internal_FindClass(constructorName);
    obj->StrongCount = 1;

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
        if (obj->Class->Destructor) {
            obj->Class->Destructor(obj);
        }

        std::free(obj);
        fprintf(stderr, "rc deallocated %s @ %p\n", className.c_str(), obj);
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
