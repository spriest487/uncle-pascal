//this is a line comment
program HelloWorld;

uses System;

type String = record
  Chars: System.Pointer;
  Length: System.Integer;
end;

type Vector = record
  Elements: System.Pointer;
  Length: System.Integer;
end;

function Vector_Create: Vector;
begin
  result.Elements := 0;
  result.Length := 0;
end;

procedure Add(self: Vector; p: System.Pointer);
var
  newElements: System.Pointer;
  i: System.Integer;
begin
  newElements := System.GetMem(self.Length + 1);

  if self.Elements <> 0 then
  begin
    for i := 0 to self.Length - 1 do
      newElements + i := self.Elements + i;

    System.FreeMem(self.Elements);
  end;

  self.Length := self.Length + 1;
  self.Elements := newElements;
end;

function Greet(name: System.String): System.String;
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

  System.WriteLn('hello world');

  if x = 1 then begin
    System.WriteLn('one')
  end
  else begin
    System.WriteLn('not one');
  end;

  //WriteLn(IntToStr(x + y));

  //WriteLn(Greet());
end.