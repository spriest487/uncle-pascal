uses System;

function GetAdder(a: Integer): function(Integer): Integer
begin
    function(b: Integer): Integer
    begin
        a + b
    end
end;

let add4 := GetAdder(4);
let z := add4(3);

WriteLn(z.ToString());