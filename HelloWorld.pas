//this is a line comment
program HelloWorld

uses System, Vector

function Greet(name: System.String): System.String
begin
  let msg := 'hello world'
  result := msg
end

var
  x: System.Integer
  y: System.Integer

  vec: Vector.Vector

begin
  x := 1
  y := 2
  x := x + y

  vec := Vector.Create()
  Vector.Add(@vec, 1)
  Vector.Add(@vec, 2)
  Vector.Add(@vec, 3)
  Vector.Add(@vec, 4)

  System.WriteLn('hello world')

  if x = 1 then begin
    System.WriteLn('one')
  end
  else begin
    System.WriteLn('not one')
  end
end.