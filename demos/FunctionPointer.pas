program FunctionPointer

uses System.*

type
    Greeter = procedure(name: String)
    Adder = function(x: Int32): Int32; cdecl

procedure HelloWorld(name: String)
    WriteLn('hello ' + name)

function AddOne(x: Int32): Int32; cdecl;
    result := x + 1

var
    greeter: Greeter
    adder: Adder
begin
    greeter := HelloWorld
    adder := AddOne

    greeter('jane')
    let added = adder(3)
    WriteLn('added: ' + StringFromInt(added))
end.