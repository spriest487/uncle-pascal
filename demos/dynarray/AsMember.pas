uses System;

type B = class
end;

type A = record
    Items: array of B;
end;

var a := A(
    Items: []
);

var b := B();

let newIndex := System.Length(a.Items);
System.SetLength(a.Items, newIndex + 1, b);

a.Items[newIndex] := b;