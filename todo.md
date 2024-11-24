# critical
* project files/don't auto load used units
* sets should only allow ranges and contiguous values (OR we should handle sparse sets somehow)
* range types/`for var x in 0..123`
* move strings to module (out of metadata)

# features
* generic interfaces 
* Object should be an interface?
  * need interface inheritance so Object can imply Comparable and Displayable
* @-attributes - replace `external` etc with these (eventually make them available at runtime but not now)
* reflection for published members
* property syntax
* numeric compiler error codes
* compound assignment bitwise ops

# bugs
* source spans shouldn't be included in module files in release mode
* parser: newlines/escape chars in string literals

# improvements
* go back to I prefix for interfaces~~~~
* undefined symbol checking can probably be moved to IR codegen rather than during typechecking 
* type cache so we don't need to construct/pass around type objects all the time in the typechecker (This is really slow)
* make IR function src spans optional?? some builtins don't have them
