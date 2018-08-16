program FunctionArgModifiers

uses System.*

procedure OutParam(out s: String)
begin
    s := 'hello jane!'
end

procedure RefParam(var i: Int32)
begin
    WriteLn('val was ' + StringFromInt(i))
    i := 1 + 1
end

var
    greeting: String
    i: Int32
begin
    OutParam(greeting)
    WriteLn(greeting)

    i := 1
    RefParam(i)
    WriteLn('val is now ' + StringFromInt(i))
end.