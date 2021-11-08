uses System;

type A = class
end;

let x := 1;
let y := A();

WriteLn('x is Any: ' + (if x is Any then true else false).ToString());
WriteLn('y is Any: ' + (if y is Any then true else false).ToString());