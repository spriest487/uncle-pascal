uses System;

type TestBox of T = class
    t: T;
end;

function TestUnwrap of T(b: TestBox of T): T
begin
    b.t
end;

let x: TestBox of Integer := TestBox(t: 123);
let y := TestUnwrap(x);