# critical
* rename type annotations again
* project files/don't auto load used units
* why is the C backend generating packed decls for everything?
* rename Any -> Object
  * maybe it should be an interface?
* take packed out of struct kind, into structdecl?
* make set types a separate thing from set decls
* enums/sets/range types (+ `in` operator)

# features
* proper function overloads (one declared name, multiple signatures)
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
* make IR function src spans optional?? some builtins don't have them
* maybe split func and type defs up in the context? for simplifying define logic
