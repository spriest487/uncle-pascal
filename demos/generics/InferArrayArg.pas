uses System;

function Array3Identity of T(a: array[3] of T): array[3] of T
begin
    a
end;

let a1 := Array3Identity([1, 2, 3]);
let a2 := [1, 2, 3].Array3Identity();