* stop outputting empty releaser funcs where there's nothing ot be released
* fmt_pretty in interpreter to format with debug type info - can default impl with Display::fmt
* CASE: fix separators - should allow optional ; before end/else
* 
* interpreter: marshalled globals
* typechecker: rework namespaces/at least the find() logic so it does imports better
* parser: newlines/escape chars in string literals
* backend: c ffi

# critical

* x[2].ToString() parses wrong!
* c codegen: escaping! can't do ""
* better message when iface impl doens't match decl sig
* add `NativeUInt` type
* unique compiler error codes
* does symbol with same name declared in branch scope confuse the initialization consolidation?
* indexing into something not indexable throws wrong error (e.g. string.chars[i])

# type inference
* for function calls (generic args from func arguments)

# big features

* enums/sets/range types
* switch statement
* match expression
* generic interfaces/generic methods
* @-attributes - replace `export`, `external` with these (eventually make them available at runtime but not now)
* proper function overloads (one declared name, multiple signatures)

# todos

* ref params should be able to refer to record fields and array elements
* defining a function needs to check visibility is the same as previous decl
* functiondecl.ident should be an enum of { FullName, MethodName }
* QualifiedDeclName should be an enum of { Raw, Specialized }

# types refactor

* iface_impls should use qualified names