# critical

* C codegen broken with undeclared functions
* extra params to method calls aren't invalid!!
* trailing comma after func param decl with random junk doesn't fail parsing
* allow trailing comma in collection init!!
* c codegen: escaping! can't do ""
* unique compiler error codes
* does symbol with same name declared in branch scope confuse the initialization consolidation?

# big features

* option/result unwrapping
* private methods
* vector list impl
* enums/sets/range types
* generic interfaces/generic methods
* @-attributes - replace `external` etc with these (eventually make them available at runtime but not now)
* proper function overloads (one declared name, multiple signatures)

# todos

* undefined symbol checking can probably be moved to IR codegen rather than during typechecking 
* properly initialize function local vars (IR)
* type cache so we don't need to construct/pass around type objects all the time in the typechecker (This is really slow)
* stop outputting empty releaser funcs where there's nothing ot be released
* parser: newlines/escape chars in string literals~~~~
