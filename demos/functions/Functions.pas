uses System;

function Add(a, b: Integer): Integer
begin
    let result: Integer := a + b;
    result
end;

let x: Integer := Add(2, 3) + 20;

WriteLn(IntToStr(x));
