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

function A.Greet(self: Impl)
begin
    WriteLn('hello from A');
end;

function B.Greet(self: Impl)
begin
    WriteLn('hello from B');
end;

function C.Greet(self: Impl)
begin
    WriteLn('hello from C');
end;

let i := Impl(value: 1);

i.Greet();