# critical

* project files/don't auto load used units
* why is the C backend generated packed decls for everything? 
* weak refs
* single-char string literals convert to Byte

# big features

* proper function overloads (one declared name, multiple signatures)
* enums/sets/range types
* generic interfaces
* @-attributes - replace `external` etc with these (eventually make them available at runtime but not now)

# todos

* source spans shouldn't be included in module files in release mode
* undefined symbol checking can probably be moved to IR codegen rather than during typechecking 
* properly initialize function local vars (IR)
* type cache so we don't need to construct/pass around type objects all the time in the typechecker (This is really slow)
* stop outputting empty releaser funcs where there's nothing ot be released
* parser: newlines/escape chars in string literals
* numeric compiler error codes
* reflection for published members
* property syntax
~~~~
