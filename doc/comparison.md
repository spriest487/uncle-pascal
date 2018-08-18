# What's different

* Semicolons are optional in every context they are required in standard Pascal.
* The object-oriented features in this dialect are unrelated and incompatible with
"Object Pascal" and other dialects, specifically:
    * There is no class inheritance, only interfaces.
    * Class members are always private, record members are always public, and
    encapsulation is always per-unit.
    * Properties do not exist
    * Methods and interface methods are declared separately to the data structure
    of the object they apply to.
    * Constructors and destructors do not exist. New instances of classes are
    created exclusively using the object initialization syntax, in the same way as
    records and arrays. Destructors are replaced with the `Disposable` interface.
    * Memory management uses automatically-managed reference counting.
* The language is case-sensitive.
* There is no exception handling or stack unwinding. The `raise` statement always
aborts the program with a diagnostic message and is intended only for unrecoverable
error states.
* There is only one builtin string type, the `System.String` class. Strings are immutable
and like other class objects, they are reference-counted. The `System` unit contains
functions (and methods of `String`) for converting to and from C-style strings
and different encodings.
* Enumerated values are namespaced by the name of the enumeration:

    // the enumerated names are Fruit.Banana and Fruit.Apple
    type Fruit = (Banana, Apple)



# Compatibility

The compiler has two compatibility modes, `fpc` and `delphi`. These modes affect
the preprocessor symbols that are defined by default. In both modes,
names and keywords are also case-insensitive.

The compatibility modes do not attempt to fully emulate other Pascal dialects and
features that are not present in this dialect such as class inheritance and properties
will still fail to compile.

In compatibility mode, enumerations

# New syntax

## Let-bindings

Names can be declared in the body of a function using the `let` keyword:

    function Greet(name: String): String
    begin
        let message = 'Hello, '
        result := message + name
    end

The type of a `let` binding is inferred from the value on the right-hand side.
An explicit type can also be provided, in which case the value on the right-hand
side must be convertible to that type:

    // 0 would be inferred as Int32 normally
    let x: Byte = 0

The values of let-bindings are immutable and cannot have their addresses taken,
and cannot be passed as `out` or `var` arguments to functions. To create a mutable
let-binding, use the `let var` variant. Note that the initial value is assigned
with the assignment operator, whereas the value of an immutable `let` uses the
equality operator:

    let var msg := 'hello'
    msg := msg + ', world!'

## For-let loop

Let-bindings can also be used in for loops, in which case the name used for the
loop counter is local to the loop body and condition:

    for let i = 0 to 100 do ...

Since loop counters are immutable, the `let var` variant cannot be used here.
