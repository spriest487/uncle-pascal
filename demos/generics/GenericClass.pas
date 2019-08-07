uses System;

type Box of Val = class
    val: Val;
end;

let y: Box of String := Box(val: 'test');
WriteLn('y contains ' + y.val);

let z: Box of Integer := Box(val: 123);
WriteLn('z contains ' + IntToStr(z.val));