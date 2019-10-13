# critical

* the code for deep-releasing structs/variants in rc types should be generated
as functions and called in IR, instead of being
dependent on the interpreter (when this functionality already
partially exists for deep-releasing local versions of the same objects)
* `type System.Option of T cannot be used in matching constructs`
* add `NativeUInt` type
* `exit` statement
* unique compiler error codes
* string comparison
* module privacy/exported functions and impls (so you can't call `String.Dispose` explicitly)
* does symbol with same name declared in branch scope confuse the initialization consolidation?
* interpreter should disallow overwriting cells on stack/heap with cells of different types (by dtor)
* unify StructID, InterfaceID, VariantID into TypeID

# type inference
* for function calls (generic args from func arguments)
* for variant ctors

# big features

* const evaluation
* enums/sets/range types
* generic types
* generic functions

# todos

* make temporary its own kind of TypeAnnotation?
* ref params should be able to refer to record fields and array elements
* defining a function needs to check visibility is the same as previous decl
* functiondecl.ident should be an enum of { FullName, MethodName }
* QualifiedDeclName should be an enum of { Raw, Specialized }
* better message when unit name conflicts with a member name (or make this not an error)

# types refactor

* iface_impls should use qualified names