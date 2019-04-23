uses System;

for let i := 1 to 10 do begin
    if i = 5 or i = 7 then begin
        continue;
    end;

    WriteLn(IntToStr(i));
end;