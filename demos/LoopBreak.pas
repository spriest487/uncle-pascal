uses System;

var counter := 0;

for let i := 1 to 10 do begin
    if i > 5 then begin
        break;
    end;

    counter := i;
end;

WriteLn(IntToStr(counter));