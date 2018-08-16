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

{ io }

procedure WriteLn(line: String)
function ReadLn(): String

{ string manipulation }

constructor StringCreate: String
constructor StringFromBytes(bytes: ^Byte; len: NativeInt): String
destructor DestroyString(string: String)

function StringFromInt(i: Int32): String
function StringToInt(s: String; outVal: ^Int32): Boolean
function StringConcat(a: String; b: String): String
function StringToCString(s: String; bytes: ^Byte; len: NativeInt): Boolean
function StringLength(s: String): NativeInt

{ native memory allocation }

function GetMem(len: NativeInt): ^Byte
procedure FreeMem(mem: ^Byte)

{ standard math functions }

function Abs(x: Float64): Float64

function Tan(x: Float64): Float64
function ArcTan(x: Float64): Float64

function Cos(x: Float64): Float64
function ArcCos(x: Float64): Float64

function Sin(x: Float64): Float64
function ArcSin(x: Float64): Float64

function Exp(x, e: Float64): Float64

function Round(f: Float64): Int32
function Ceil(f: Float64): Int32
function Floor(f: Float64): Int32
function Trunc(f: Float64): Int32

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

function StringToCString(s: String; bytes: ^Byte; len: NativeInt): Boolean
begin
    if len < s.Length + 1 then
        result := false
    else begin
        for let i = 0 to s.Length do
            ^(bytes + i) := ^(s.Chars + i)

        ^(bytes + s.Length) := #0
        result := true
    end
end

function StringLength(s: String): NativeInt
begin
    result := s.Length
end

end.