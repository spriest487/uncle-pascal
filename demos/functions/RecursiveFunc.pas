uses System;

function Recurs(i: Integer): Integer
begin
    if i < 5 then Recurs(i + 1) else i
end;

let five := Recurs(0);
WriteLn(IntToStr(five));