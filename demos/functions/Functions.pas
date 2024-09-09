implementation
uses System;

function Add(a, b: Integer): Integer
begin
    let result: Integer := a + b;
    result
end;

initialization
    var x: Integer := Add(2, 3) + 20;

    WriteLn(x.ToString());
end
