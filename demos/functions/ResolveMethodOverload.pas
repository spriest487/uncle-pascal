implementation
uses System;

type Greeter = class
    function Greet(a: Integer); overload;
    function Greet(b: Boolean); overload;
end;

{
function Greeter.Greet(a: Integer);
begin
    WriteLn('hello from A: ' + a);
end;

function Greeter.Greet(b: Boolean);
begin
    WriteLn('hello from B: ' + b);
end;
}

initialization
    var i := Greeter();
    i.Greet(1);
    i.Greet(true);
end
