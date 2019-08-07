uses System;

type Box of Val = class
    val: Val;
end;

function UnwrapBox of T(box: Box of T): T
begin
    box.val
end;

let box: Box of Integer := Box(val: 1);
let val := UnwrapBox of Integer(box);

WriteLn(IntToStr(val));
