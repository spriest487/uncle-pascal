program Consts

uses
    System.*

const
    NUM = 1 - 999
    YES = true
    NO: Boolean = false
    UNSIGNED_SOMETHING = $DEADBEEF
    CHAR_SOMETHING = #32
    FLOAT_SOMETHING = 1.0
    FLOAT_BIG = 9.999E7
    FLOAT_ONE = 1.0; FLOAT_TWO = 3E1

function Test
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