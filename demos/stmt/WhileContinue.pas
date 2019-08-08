uses System;

var x := 0;
while x < 5 do begin
    x := x + 1;
    if x = 3 then continue;

    WriteLn('x is now: ' + IntToStr(x));
end;
