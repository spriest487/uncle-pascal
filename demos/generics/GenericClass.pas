uses System;

type GenericBox[Val] = class
    val: Val;
end;

let y: GenericBox[Integer] := GenericBox(val: 123);
WriteLn('y contains ' + IntToStr(y.val));

let z: GenericBox[String] := GenericBox(val: 'hello world');
WriteLn('z contains ' + z.val);