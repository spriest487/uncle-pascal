unit CallWithoutArgsDemo;

interface

implementation

uses System;

function One: Integer;
begin
    1;
end;

function Two(x: Integer): Integer;
begin
    x + 1
end;

function WriteA;
begin
    WriteLn('A');
end;

initialization

begin
    // direct call to function
    var a := One;
    WriteLn('a: ' + a.ToString());

    // ufcs call
    var b := (1 as Integer).Two;
    WriteLn('b: ' + b.ToString());
end;

end.