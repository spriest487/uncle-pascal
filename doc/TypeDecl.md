Aggregate types (variants, records and object classes) may refer to
themselves in their own definitions.

The total size of a type must always be known. The size of a type is
unknown until the end of its own definition block, so it may only be
used in context where the size is not required. For example, a record
may not have a field of its own type, but it may have a field of the
pointer of its own type.  

The exception to this is classes. Class variables are references to the
class instance, so always have the same size as a pointer.