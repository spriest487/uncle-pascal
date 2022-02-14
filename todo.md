* auto reference units by search path instead of requiring them all to be named on the cli
* match! for events
* stop outputting empty releaser funcs where there's nothing ot be released
* fmt_pretty in interpreter to format with debug type info - can default impl with Display::fmt
* interpreter: marshalled globals
* typechecker: rework namespaces/at least the find() logic so it does imports better
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
* @-attributes - replace `export`, `external` with these (eventually make them available at runtime but not now)
* proper function overloads (one declared name, multiple signatures)

# todos

* functiondecl.ident should be an enum of { FullName, MethodName }
* QualifiedDeclName should be an enum of { Raw, Specialized }
