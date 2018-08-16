//this is a line comment
program PreprocessorDemo

uses System.*

(* this is another comment *)
(***
this kind of comment can be multiline
***)

{ so can
this kind }

{$define friendly}

function Greet(name: String): String
begin
  {$ifdef friendly}
  let greeting = 'hi '
  {$else}
  let greeting = 'hello '
  {$endif}

  let msg = greeting + name

  {$ifndef friendly}
  result := msg
  {$else}
  result := msg + '!'
  {$endif}
end

begin
    WriteLn(Greet('jane'))
end.