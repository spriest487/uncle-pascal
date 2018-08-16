program Consts

uses
    System.*

const
    NUM = 1
    YES = true
    NO: Boolean = false

procedure Test
const
    NAME = 'jane'
    HELLO = 'hello'
    GREETING: String = HELLO
begin
    WriteLn(GREETING + ' ' + NAME + ', num is ' + StringFromInt(NUM))
end

begin
    if YES then
        Test()
end.