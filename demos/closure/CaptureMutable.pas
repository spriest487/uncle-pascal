uses System;

var x := 1;

let f := function(): String
begin
    x += 1;
    x.ToString()
end;

WriteLn(f());
WriteLn(f());
WriteLn(f());