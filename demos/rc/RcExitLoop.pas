uses System;

function Greet(name: String)
begin
    for let i := 1 to 5 do begin
        let tmpStr := 'hello' + name;
        exit;
    end;
end;

Greet('world');