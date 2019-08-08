uses System;

var x := 0;

while true do begin
    if x = 5 then break;

    x := x + 1;
end;

WriteLn('x is now: ' + IntToStr(x));