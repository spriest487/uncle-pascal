uses System;

function Array3Identity(a: array[3] of Integer): array[3] of Integer
begin
    a
end;

function DynArrayIdentity(a: array of Integer): array of Integer
begin
    a
end;


let a1 := Array3Identity([1, 2, 3]);
let a2 := [1, 2, 3].Array3Identity();

let a3 := DynArrayIdentity([1, 2, 3]);
let a4: array of Integer := [1, 2, 3];
let a5 := a4.DynArrayIdentity();