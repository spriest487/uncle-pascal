//this is a line comment
program HelloWorld

uses System.*, Vector.Vector

{$define friendly}

function Greet(name: String): String
begin
  {$ifdef friendly}
  let greeting := 'hi '
  {$else}
  let greeting := 'hello '
  {$endif}

  let msg := greeting + name

  {$ifndef friendly}
  result := msg
  {$else}
  result := msg + '!'
  {$endif}
end

var
  //vec: Vector

begin
  let x := 1
  let y := 2
  x := x + y

  let vec := Vector.Create()
  vec.Add(1)
  vec.Add(2)
  Vector.Add(vec, 3)
  Vector.Add(vec, 4)

  let name := 'wuuuorld'

  WriteLn(Greet(name))

  if x = 1 then begin
    WriteLn('one')
  end
  else begin
    WriteLn('not one')
  end
end.