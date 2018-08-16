program Consts

uses
    System.*

const num = 1
    yes = true
    no = false

procedure Test
const
    name = 'jane'
begin
    WriteLn('hello ' + name + ', num is ' + StringFromInt(num))
end

begin
    if yes and not no then
        Test()
end.