# Memory management

Memory is managed automatically for class types. Memory management uses
reference counting which means allocations happen in a determined and
object destruction happens predictably, but it is possible to leak memory
by creating cycles.

1. ## Retaining references

    1. The value of a reference is retained as long as one or more references
    to it exist. Assigning an existing reference to a new variable, local
    binding or field increases the number of active references. 
    
    1. A reference exists until it goes out of scope or the object containing 
    it is itself destroyed.

    1. As soon as there are no more references, the referenced object destroyed.

        ```pascal
        begin
            let cat = Cat(
                owner: Human(Name: 'Jane')
            )
            // cat and cat.Human both exist
        end
        //cat no longer exists and both objects are destroyed
        ```

    1. When an object is destroyed, its implementation of `System.Disposable.Dispose`,
    if present, is called. After this the object is considered disposed and
    must not used again.
    1. If the `Dispose` implementation creates any new references to the object
    being disposed that are still live after the method exits, a fatal error
    is raised and the application exits.
    1. If an object has any live references to other objects when it is destroyed,
    those objects are disposed (in field order) before the owning object is
    disposed.

1. ## Breaking cycles

    1. To store cyclical references without leaking memory, weak references exist.
    These differ from normal references in several ways:

        1. They do not contribute to the count of active references
        1. They can be assigned `nil` as a value
        1. They can be compared to `nil` to determine if the object they reference has
        been destroyed

    1. Weak references are declared with the `weak` contextual keyword before the type
    name in a declaration. They can be used in the following contexts:
        * Local variables (`let`, `let var` or `var` sections)
        * Record and class fields
        * As array elements in any of the above contexts

        Weak references are unsuitable for generally representing optional
        values. They can not be used as function arguments or return values, 
        except `out` and `ref` arguments.

    ```pascal
    let var myCat: weak Cat := nil
    // (myCat = nil) = true

    begin
        let cat = Cat()
        myCat := cat

        // `myCat` references the same object as `cat`
        // (myCat <> nil) = true
    end
    // `cat` no longer exists
    // (myCat = nil) = true
    ```
