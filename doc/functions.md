# Methods

Methods are functions associated with a type. A function with at least one argument,
where the first argument named `self`, is a method associated with the type of that argument.

    // method of Dog
    function Bark(self: Dog)

    // method of Int32
    function Add(self, x: Int32): Int32

Methods can declared in any unit, regardless of the type they are associated with.

Methods are not added to the unit namespace. Instead, they are namespaced under the
name of the associated type:

    // error
    //Bark(spot)
    //Add(1, 2)

    // correct
    Dog.Bark(spot)
    Int32.Add(1, 2)


Using this syntax, a method can be called anywhere the function would normally be visible.

Methods can be called on instances of their associated type directly, in which case
the target object is passed as the `self` argument and that argument is omitted from the
argument list:

    spot.Bark()
    1.Add(2)

Using this syntax, methods can be called from the unit they are declared in and any unit
which imports the method by name in its `uses` section:

    uses Dogs.Dog.* // can now call dog.Bark() here

    // or

    uses Dogs.Dog.*

    // or

    uses Dogs.*

# Interfaces

Interfaces are types composed of a group of methods. An interface is declared in a type
declaration, listing a name and function signature for each method. Each function signature
must take at least one parameter, and the first parameter must be of the placeholder type `Self`.

    type Walkable = interface
        function Walk(self: Self)
        function NeedsWalk(self: Self; lastWalked: DateTime): Boolean
    end

`Self` is a special type which is substituted for the actual type in each implementation.

An interface can be implemented for a type by declaring functions with the same signature
as the methods of the interface. Interface methods must also be explicitly prefixed with the name
of the interface:

    function Walkable.Walk(self: Dog)

Interface methods can only be declared in the same unit as the type they are declared for.

An type that has an implementation declared for one method of an interface must also declare
implementations for all other methods, if any.

Interface methods can be called as normal functions by qualifying their name with the name
of the interface:

    Walkable.Walk(spot)

If the interface name is imported into the unit in its `uses` section, the method syntax
can be used too:

    uses Animals.Walkable

    spot.Walk()

Interfaces types can be used as the type of object members, variables and function parameters.
Values with an interface type use dynamic method calls and can hold any class object that
implements the interface. Value objects including records cannot be stored in interface
values.

    // cat and dog are Walkable classes
    let var pet: Walkable = Dog()
    pet.Walk()

    pet := Cat()
    pet.Walk()

## Special interfaces

### `System.Disposable`

The interface `System.Disposable` is declared as follows:

    interface
        function Dispose(self: Self)
    end

If a class type implements this interface, its `Dispose()` implementation will be invoked on an
instance as soon as there are no more references to the object, before the object is
deallocated.

### `System.ToString`

The interface `System.ToString` is declared as follows:

    interface
        function ToString(self: Self): System.String
    end

Types that implement `ToString` will have this method called automatically if they are
used in string concatenation expressions:

    function ToString.ToString(self: Dog): String
    begin
        result := self.Name
    end

    // prints "good boy, Spot"
    WriteLn('good boy, ' + Dog(Name: 'Spot'))

# Compatibility

The keyword `procedure` is a valid synonym for `function` where the function has
no result type. Using the `procedure` keyword and declaring a result type is an error.