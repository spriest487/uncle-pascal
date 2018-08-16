# TODO

## urgent/in progress
* => function syntax
* add Name node in C++ AST for renaming (--> remove pascal names from C++ output)
* shadowing breaks initialization checks which are done by name
* RC struct members on the stack

## fixes
* variant fields with the same name
* Implement suffix operators (for ^)
* weak variables to break RC cycles
* "uses" should be able to appear at any position in the decls

## missing features
* set literals
* sets supporting ..
* sets of numeric types
* variable length arrays
* implement `exit` and make arc work with alterate control flows

## nice
* Reflection
* string formatting
* Library compilation
* Library imports/exports & external functions
* Unsafe
    * pointers
    * GetMem/FreeMem
    * nil
* feature switches for extended/deprecated syntax
    * deprecate 'with'
    * deprecate 'procedure'
    * deprecate function 'var'
    * deprecate untyped var
    * deprecate variable-length arrays