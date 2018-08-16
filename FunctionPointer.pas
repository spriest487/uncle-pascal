program FunctionPointer

uses System.*

type
    Greeter = procedure(name: String)
    Adder = function(x: Int32): Int32

procedure HelloWorld(name: String) 
begin
    WriteLn('hello ' + name)
end

function AddOne(x: Int32): Int32
begin
    result := x + 1
end

var
    greeter: Greeter
    adder: Adder
begin
    greeter := HelloWorld
    adder := AddOne

    greeter('jane')
    adder(3)
end.