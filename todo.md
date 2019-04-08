# critical

* add `NativeUInt` type
* `exit` statement
* while loop
* unique compiler error codes
* any-references
* string comparison
* module privacy/exported functions and impls (so you can't call `String.Dispose` explicitly)
* parse failure: can't distinguish Ctor() expr with 0 args from a function call
* does symbol with same name declared in branch scope confuse the initialization consolidation?
* interpreter should disallow overwriting cells on stack/heap with cells of different types (by dtor)

# big features

* const evaluation
* variants
* enums/sets/range types
* generic types
* generic functions
* c backend

# todos

* IR formatter with type/binding names from metadata
* Binding.def used? 
* make temporary its own kind of TypeAnnotation?
* ref params should be able to refer to record fields and array elements
* expr translations should take an out ref as a param instead of creating and returning one
    (this shouldn't break RC though, do we ever return non-temp refs)
* remove span from Ident (it should be part of TokenTree, Expression but not Ident itself)