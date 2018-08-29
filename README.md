# Links

[Local documentation](doc/index.md)

# TODO

## urgent/in progress
* error on undefined functions
* shadowing breaks initialization checks which are done by name

## fixes
* variant fields with the same name
* Implement suffix operators (for ^)
* "uses" should be able to appear at any position in the decls
* trying to call a function which doesn't exist via ufcs on a class
    shows the wrong error message (member cannot be accessed)
* external functions in the Implementation sections don't work

## missing features
* set literals
* sets supporting ..
* sets of numeric types
* variable length arrays
* forward functions in implementation section

## nice
* Get rid of reliance on C++ collections so the backend emits plain C again
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