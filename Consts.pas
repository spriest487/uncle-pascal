program Consts

uses
    System.*

procedure Test
const
    num = 1
    name = 'jane'
begin
    WriteLn('hello ' + name + ', num is ' + StringFromInt(num))
end

begin
    Test()
end.