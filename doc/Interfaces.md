Interfaces declarations can make use of the `Self` type, which is an undefined, unsized
placeholder type that stands in the for the implementing type.

For example:

```pascal
type IAddToSelf = interface
    function Add(other: Self): Self;
end;

type Value = class of IAddToSelf
    function Add(other: Value): Value;
end;
```

Interfaces can be declared with type params (NYI).

```pascal
type IPerson[Job] = interface
    function DoWork(self: Self; job: Job);
end;
```

Interface methods cannot have type params.
It must be possible to fully instantiate an
interface type at compile-time, which means
that the signature of all of its methods must
be known in advance.

```pascal
type IPerson = interface
    // Not valid:
    function DoWork[Job](self: Self; job: Job);
end;
```
