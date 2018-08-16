//this is a line comment
program HelloWorld

(* this is another comment *)
(***
this kind of comment can be multiline
***)

{ so can
this kind }

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
  ints: array [0..2] of Int32
  ints2: array[-1..1, -2..2] of Int32

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