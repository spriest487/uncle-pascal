uses System;

var x := 0;

while true do begin
    x := x + 1;

    if x = 5 then break;
end;

WriteLn('x: ' + IntToStr(x));