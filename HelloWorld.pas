//this is a line comment
program HelloWorld

uses System, Vector

function Greet(name: ^System.String): ^System.String
begin
  let msg := 'hello ' + name + '!'
  result := msg
end

var
  vec: ^Vector.Vector

begin
  let x := 1
  let y := 2
  x := x + y

  vec := Vector.Create()
  vec.Add(1)
  vec.Add(2)
  Vector.Add(vec, 3)
  Vector.Add(vec, 4)

  System.WriteLn(Greet('wuuuorld'))

  if x = 1 then begin
    System.WriteLn('one')
  end
  else begin
    System.WriteLn('not one')
  end
end.