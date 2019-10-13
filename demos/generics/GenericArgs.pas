uses System;

type TestBox of Val = class
    val: Val;
end;

function UnwrapTestBox of T(box: TestBox of T): T
begin
    box.val
end;

let box: TestBox of Integer := TestBox(val: 1);
let val := UnwrapTestBox of Integer(box);

WriteLn(IntToStr(val));
