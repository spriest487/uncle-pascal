uses System;

function A(): Boolean
begin
    let a: Integer := begin
        exit true;
    end;

    false
end;

let result := A();
WriteLn(result.ToString());