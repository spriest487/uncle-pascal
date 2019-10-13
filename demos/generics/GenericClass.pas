// uses System;

type GenericBox of Val = class
    val: Val;
end;

let y: GenericBox of Boolean := GenericBox(val: true);
// WriteLn('y contains ' + y.val);

let z: GenericBox of Integer := GenericBox(val: 123);
// WriteLn('z contains ' + IntToStr(z.val));