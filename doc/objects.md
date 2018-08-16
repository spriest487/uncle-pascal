# Records

Records are value objects. They are stored locally and require no memory management.
A record type is defined in a type declaration. The record declaration contains one or more field declarations.

    type Cat = record
        Color: Color
        NumLegs: Int32
    end

A variant part can be declared in addition to or in place of the normal field list.
If present, the variant part must be the last item in the record declaration.
The variant part consists of a variant tag which identifies the variant an object
holds, which otherwise acts like a normal field.

    type Animal = record
        case Kind: AnimalKind of
            AnimalKind.Cat: (
                Whiskers: Int64
            )
            AnimalKind.Bird: (
                Speaks: Boolean
                Color: Color
            )
    end

A record object is instantiated using the object constructor syntax.

    let cat = Cat (
        Color: Color.Red
        NumLegs: 4
    )

    // constructor type can also be inferred from the left-hand side
    let cat: Cat = ( Color: Color.Brown; NumLegs: 3 )

    // variants can be constructed this way too
    let animal = Animal (
        // defaults to the first variant if not specified
        Kind: AnimalKind.Cat

        // can only construct fields for the current variant
        Whiskers: 999

        // error
        //Speaks: true
    )

* The value of a record object is public. They can be referenced, instantiated and their
members can be accessed and mutated from any unit.
* Record objects are passed by value and copied where necessary. A variable of a record
type holds the record object itself and is large enough to store any variant of the record.
* The value of a record object can be passed by reference as normal using the
`out` and `var` function argument modifiers.

# Classes

Classes are reference objects. They are stored in global memory and are disposed when
there are no more references to them.

A class type is defined similarly to a record. A class may also have a variant part.

    type Dog = class
        Name: String
        Spots: Int32
    end

A class object is instantiated in the same way.

    let dog = Dog (
        name: 'Spot'
        spots: 0
    )

* The value of a class object is private. They can be referenced by functions in any unit,
but their fields are inaccessible to functions in other units, and they can't be
constructed in other units.
* Class objects have a unique identity that does not change until the object is disposed of.
Mutating the fields of the object or changing the variant tag does not change its identity.
* Class objects are always passed by reference. A class object passed as an `out` or
`ref` parameter is passed as a reference to a reference, and may change the identity of the
object.