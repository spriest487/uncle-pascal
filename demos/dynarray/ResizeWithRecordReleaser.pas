uses System;

type A = record
    value: String;
end;

let as: array of A := [];

let a := A(value: 'hello');
SetLength(as, 1, a);