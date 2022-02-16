* type cache so we don't need to construct/pass around type objects all the time in the typechecker (This is really slow)
* stop outputting empty releaser funcs where there's nothing ot be released
* interpreter: marshalled globals
* parser: newlines/escape chars in string literals
* backend: c ffi

# critical

* c codegen: escaping! can't do ""
* better message when iface impl doens't match decl sig
* unique compiler error codes
* does symbol with same name declared in branch scope confuse the initialization consolidation?

# big features

* enums/sets/range types
* match expression
* generic interfaces/generic methods
* @-attributes - replace `external` etc with these (eventually make them available at runtime but not now)
* proper function overloads (one declared name, multiple signatures)

# todos

* functiondecl.ident should be an enum of { FullName, MethodName }
* QualifiedDeclName should be an enum of { Raw, Specialized }
