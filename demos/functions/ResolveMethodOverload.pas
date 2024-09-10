implementation
uses System;

type A = interface
    function Greet(self: Self; a: Integer);
end;

type B = interface
    function Greet(self: Self; b: Boolean);
end;

type Impl = class
end;

function Greet of A(self: Impl; a: Integer);
begin
    WriteLn('hello from A');
end;

function Greet of B(self: Impl; b: Boolean);
begin
    WriteLn('hello from B');
end;

initialization
    var i := Impl();
    i.Greet(1);
    i.Greet(true);
end
