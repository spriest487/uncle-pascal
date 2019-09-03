Interfaces can have type params (`NYI`).

```pascal
type IPerson of Job = interface
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
    { Error }
    function DoWork of Job(self: Self; job: Job);
end;
```