program Consts

uses
    System.*

const
    NUM = 1 - 999
    YES = true
    NO: Boolean = false
    UNSIGNED_SOMETHING = $DEADBEEF
    CHAR_SOMETHING = #32

procedure Test
const
    NAME = 'ja' + 'ne'
    HELLO = 'hello'
    GREETING: String = HELLO
begin
    WriteLn(GREETING + ' ' + NAME + ', num is ' + StringFromInt(NUM))
end

begin
    if YES then
        Test()
end.