type Box of T = class
    val: T;
end;

function BoxUp of T(val: T): Box of T
begin
    Box(val: val)
end;

let i := 1;
let iBox := BoxUp of Integer(i);

System.WriteLn(System.IntToStr(iBox.val));