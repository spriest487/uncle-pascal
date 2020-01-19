uses System;

type I = interface
    function First(a: Self; b: Integer);
    function Second(a: Integer; b: Self);
    function Both(a, b: Self);
end;

type C = class
end;

function I.First(a: C; b: Integer)
begin
    WriteLn('first');
end;

function I.Second(a: Integer; b: C)
begin
    WriteLn('first');
end;

function I.Both(a, b: C)
begin
    WriteLn('first');
end;

let c := C();
I.First(c, 123);
I.Second(456, c);
I.Both(c, c);

