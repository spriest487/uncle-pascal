# critical

* add `NativeUInt` type
* `exit` statement
* while loop
* unique compiler error codes
* string comparison
* module privacy/exported functions and impls (so you can't call `String.Dispose` explicitly)
* does symbol with same name declared in branch scope confuse the initialization consolidation?
* interpreter should disallow overwriting cells on stack/heap with cells of different types (by dtor)
* use ids for methods instead of names
* unify StructID, InterfaceID, VariantID into TypeID

# big features

* const evaluation
* enums/sets/range types
* generic types
* generic functions

# todos

* Binding.def used?
* make temporary its own kind of TypeAnnotation?
* ref params should be able to refer to record fields and array elements
* defining a function needs to check visibility is the same as previous decl
* functiondecl.ident should be an enum of { FullName, MethodName }
* QualifiedDeclName should be an enum of { Raw, Specialized }

# types refactor

* Store defs in Context
* Store type/func instantiations in Context instead of generating them on the fly in IR generation
* patterns and other variant exprs should use ident instead of index to identify the case
* iface_impls should use qualified names
* remove class refs from types
* recursive types!