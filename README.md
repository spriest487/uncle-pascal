# TODO

## urgent/in progress
* => function syntax
* add Name node in C++ AST for renaming (--> remove pascal names from C++ output)
* shadowing breaks initialization checks which are done by name
* RC struct members on the stack
* Type hints for expr_type to help infer types for constant expressions and make
    dealing with uints less painful

## fixes
* variant fields with the same name
* Implement suffix operators (for ^)
* weak variables to break RC cycles
* "uses" should be able to appear at any position in the decls
* trying to call a function which doesn't exist via ufcs on a class
    shows the wrong error message (member cannot be accessed)
* external functions in the Implementation sections don't work

## missing features
* set literals
* sets supporting ..
* sets of numeric types
* variable length arrays
* implement `exit` and make arc work with alterate control flows
* forward functions in implementation section

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