unit CallWithoutArgsDemo;

interface

implementation

uses System;

function One: Integer;
begin
    1;
end;

function WriteA;
begin
    WriteLn('A');
end;

initialization

begin
    var x := function();
      begin
        WriteLn('B');
      end;
    x();
end;

end.