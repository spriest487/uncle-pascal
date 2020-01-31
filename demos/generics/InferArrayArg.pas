uses System;

function Array3Identity of T(a: array[3] of T): array[3] of T
begin
    a
end;

function OuterFunc of X()
begin
    // this should never report "expected T", we should only ever try to typecheck against real signatures
    // and if we can't figure out ahead of time which one it is, we should be resolving the signature by evaluating
    // arguments
    Array3Identity([1, 2, 3]);
    //[1, 2, 3].Array3Identity();
end;

OuterFunc of String();

