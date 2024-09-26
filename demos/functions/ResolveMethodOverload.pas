implementation
uses System;

type A = interface
    function Greet(a: Integer);
end;

type B = interface
    function Greet(b: Boolean);
end;

type Impl = class of A, B
    function Greet(a: Integer); overload;
    function Greet(b: Boolean); overload;
end;

function Impl.Greet(a: Integer);
begin
    WriteLn('hello from A');
end;

function Impl.Greet(b: Boolean);
begin
    WriteLn('hello from B');
end;

initialization
    var i := Impl();
    i.Greet(1);
    i.Greet(true);
end
