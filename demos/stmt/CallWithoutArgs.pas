implementation
uses System;

function One: Int32;
begin
    1;
end;

function Two(x: Int32): Int32;
begin
    x + 1
end;

function WriteA;
begin
    WriteLn('A');
end;

type C = interface
    function M(self: Self): Int32;
end;

function C.M(self: Int32): Int32;
begin
    self + 1
end;

initialization
    // direct call to function
    var a := One;
    WriteLn('a: ' + a);

    // ufcs call
    var b := (1 as Int32).Two;
    WriteLn('b: ' + b);

    // todo: doesn't work yet because this resolves as an overload
    // method call
    var c := (2 as Int32).M;
    WriteLn('c: ' + c);
end.
