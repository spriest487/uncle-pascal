uses System;

type Box<Val> = class
    val: Val;
end;

let y: Box<String> := Box(val: 'test');
WriteLn('y contains ' + y.val);

let z: Box<Integer> := Box(val: 123);
WriteLn('z contains ' + IntToStr(z.val));