program HelloWorld

uses System.*, Vector.Vector

function Greet(name: String): String
begin
    result := 'hello, ' + name + '!'
end

var
  //vec: Vector
  ints: array [0..2] of Int32
  ints2: array[-1..1, -2..2] of Int32

begin
  let var y := 2
  StringToInt('1', y)

  let x = 1 + y

  let vec = Vector.New()
  vec.Add(1)
  vec.Add(2)
  Vector.Add(vec, 3)
  Vector.Add(vec, 4)

  let name = 'world'

  WriteLn(Greet(name))

  if x = 1 then begin
    WriteLn('one')
  end
  else begin
    WriteLn('not one: ' + StringFromInt(x))
  end
end.