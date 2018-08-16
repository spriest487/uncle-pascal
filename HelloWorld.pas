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
  result.Elements := nil;
  result.Length := 0;
end;

procedure Vector_Add(self: Vector; p: Pointer);
var
  newElements: Pointer;
  i: Integer;
begin
  newElements := GetMem(self.Length + 1);

  if self.Elements <> nil then
  begin
    for i := 0 to self.Length - 1 do
      (newElements + i) := self.Elements + i;

    FreeMem(self.Elements);
  end;

  self.Length := self.Length + 1;
  self.Elements := newElements;
end;

//function malloc(System.Integer): System.Pointer;
//function free(System.Pointer);

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

  WriteLn('hello world');

  if x = 1 then begin
    WriteLn('one')
  end
  else begin
    WriteLn('not one');
  end;

  //WriteLn(IntToStr(x + y));

  //WriteLn(Greet());
end.