implementation
uses System;

type A = interface
    function Greet(self: Self);
end;

type B = interface
    function Greet(self: Self);
end;

type C = interface
    function Greet(self: Self);
end;

type Impl = class
    value: Integer;
end;

function Greet of A(self: Impl);
begin
    WriteLn('hello from A');
end;

function Greet of B(self: Impl);
begin
    WriteLn('hello from B');
end;

function Greet of C(self: Impl);
begin
    WriteLn('hello from C');
end;

initialization
    var i := Impl(value: 1);
    
    i.Greet();
end
