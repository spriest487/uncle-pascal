program HelloWorld;

uses NPascal.System;

function Greet: String;
var
  msg: String;
begin
  msg := 'hello world';
  result := msg;
end;

var
  x: Integer;
  y: Integer;
begin
  x := 1;
  y := 2;
  x := x + y;

  WriteLn(IntToStr(x + y));

  WriteLn(Greet());
end.