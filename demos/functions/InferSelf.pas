implementation
uses System;

type I = interface
    function First(a: Self; b: Integer);
    function Second(a: Integer; b: Self);
    function Both(a, b: Self);
end;

type C = class
end;

function First of I(a: C; b: Integer);
begin
    WriteLn('first');
end;

function Second of I(a: Integer; b: C);
begin
    WriteLn('second');
end;

function Both of I(a, b: C);
begin
    WriteLn('both');
end;

initialization
    var c := C();
    I.First(c, 123);
    I.Second(456, c);
    I.Both(c, c);
    
    c.First(123);
    c.Both(c);
end
