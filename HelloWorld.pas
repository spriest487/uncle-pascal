//this is a line comment
program HelloWorld;

uses System;

function Greet: System.String;
var
  msg: System.String;
begin
  msg := 'hello world';
  result := msg;
end;

var
  x: System.Integer;
  y: System.Integer;
begin
  x := 1;
  y := 2;
  x := x + y;

  WriteLn('hello world');

  if x = 1 then begin
    WriteLn('one')
  end
  else begin
    WriteLn('not one');
  end

  //WriteLn(IntToStr(x + y));

  //WriteLn(Greet());
end.