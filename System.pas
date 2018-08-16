unit System

interface

{$ifdef fpc}
type
    Cardinal = UInt32
    Integer = Int32
    LongWord = UInt32
    Single = Float64
    Double = Float64
{$endif}

type
    String = class
        Chars: ^Byte
        Length: NativeInt
    end

procedure WriteLn(line: String)

constructor StringCreate: String
constructor StringFromBytes(bytes: ^Byte; len: NativeInt): String
destructor DestroyString(string: String)

function StringFromInt(i: Int32): String
function StringToInt(s: String; outVal: ^Int32): Boolean
function StringConcat(a: String; b: String): String

function GetMem(len: NativeInt): ^Byte
procedure FreeMem(mem: ^Byte)

implementation

constructor StringCreate: String
begin
    result.Chars := nil
    result.Length := 0
end

constructor StringFromBytes(bytes: ^Byte; len: NativeInt): String
begin
    result.Chars := GetMem(len)
    result.Length := len

    for let c = 0 to result.Length do
        ^(result.Chars + c) := ^(bytes + c)
end

destructor DestroyString(string: String)
begin
    if string.Length > 0 then
    begin
        FreeMem(string.Chars)

        string.Chars := nil
        string.Length := 0
    end
end

function StringConcat(a: String; b: String): String
begin
    if a.Length = 0 and b.Length = 0 then
        result := StringCreate()
    else if a.Length = 0 then
        result := StringFromBytes(b.Chars, b.Length)
    else if b.Length = 0 then
        result := StringFromBytes(a.Chars, a.Length)
    else begin
        result := StringCreate();

        result.Length := a.Length + b.Length;
        result.Chars := GetMem(result.Length);

        for let c = 0 to a.Length do
            ^(result.Chars + c) := ^(a.Chars + c);

        for let c = 0 to b.Length do
            ^(result.Chars + a.Length + c) := ^(b.Chars + c);
    end;
end

end.