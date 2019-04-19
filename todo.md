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
