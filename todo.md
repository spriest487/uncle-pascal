# critical
* !! functions need their decl sig to identify which overload to call, but their call decl for stuff like return type
  * maybe just revert some of the changes and make sure it uses the specialized return ty instead of the decl one

* generate bounds checks as a separate function instead of inlining them
* remove baked in type arg position info
* unify type param constraint struct style (use Any)
* `Self` only works properly when used directly as a function param, not if it's contained in other types
  * limit self to inferface method decls, don't allow it as a general alias for the current type now that forward
    decls are working properly
* maybe split func and type defs up in the context? for simplifying define logic
* project files/don't auto load used units
* why is the C backend generating packed decls for everything? 
* weak refs
* single-char string literals convert to Byte

# features
* proper function overloads (one declared name, multiple signatures)
* enums/sets/range types
* generic interfaces
* @-attributes - replace `external` etc with these (eventually make them available at runtime but not now)
* reflection for published members
* property syntax
* numeric compiler error codes

# bugs
* source spans shouldn't be included in module files in release mode
* parser: newlines/escape chars in string literals

# investigate
* properly initialize function local vars (IR) (??)

# improvements
* undefined symbol checking can probably be moved to IR codegen rather than during typechecking 
* type cache so we don't need to construct/pass around type objects all the time in the typechecker (This is really slow)
* stop outputting empty releaser funcs where there's nothing ot be released

