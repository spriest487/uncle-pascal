program FunctionArgModifiers

uses System.*

function OutParam(out s: String)
    s := 'hello jane!'

function RefParam(var i: Int32)
begin
    WriteLn('val was ' + StringFromInt(i))
    i := 1 + 1
end

var
    greeting: String
    i: Int32
begin
    OutParam(greetingl)
    WriteLn(greeting)

    i := 1
    RefParam(i)
    WriteLn('val is now ' + StringFromInt(i))
end.