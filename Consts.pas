program Consts

uses
    System.*

const num = 1

procedure Test
const
    name = 'jane'
begin
    WriteLn('hello ' + name + ', num is ' + StringFromInt(num))
end

begin
    Test()
end.