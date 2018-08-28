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
        Length: NativeUInt
    end

    Disposable = interface
        function Dispose(self: Self)
    end

{ io }

function WriteLn(line: String)
function ReadLn(): String

{ string manipulation }

function StringCreate: String
function StringFromBytes(bytes: ^Byte; len: NativeUInt): String
function Disposable.Dispose(string: String)

function StringFromInt(i: Int32): String
function StringToInt(s: String; out val: Int32): Boolean
function Concat(self: String; b: String): String
function ToCString(self: String; bytes: ^Byte; len: NativeUInt): Boolean
function Length(self: String): NativeUInt

{ native memory allocation }

function GetMem(len: NativeUInt): ^Byte
function FreeMem(mem: ^Byte)

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

function StringCreate: String = (Chars: nil; Length: 0)

function StringFromBytes(bytes: ^Byte; len: NativeUInt): String
begin
    let var chars := GetMem(len)

    for let c = NativeUInt(0) to len do
        chars[c] := bytes[c]

    exit ( Chars: chars; Length: len )
end

function Disposable.Dispose(string: String)
begin
    if string.Length = NativeUInt(0) then exit

    if string.Chars = nil then
        raise 'string with length > 0 must be initialized'

    FreeMem(string.Chars)

    string.Chars := nil
    string.Length := 0
end

function Concat(self: String; b: String): String
begin
    if self.Length = 0 and b.Length = 0 then exit StringCreate()
    if self.Length = 0 then exit StringFromBytes(b.Chars, b.Length)
    if b.Length = 0 then exit StringFromBytes(self.Chars, self.Length)

    let length = self.Length + b.Length;
    let var chars := GetMem(length);

    for let c = NativeUInt(0) to self.Length do
        chars[c] := self.Chars[c];

    for let c = NativeUInt(0) to b.Length do
        chars[self.Length + c] := b.Chars[c];

    exit ( Chars: chars; Length: length )
end

function ToCString(self: String; bytes: ^Byte; len: NativeUInt): Boolean
begin
    if len < self.Length + NativeUInt(1) then
        exit false

    for let i = NativeUInt(0) to self.Length do
        bytes[i] := self.Chars[i]

    bytes[self.Length] := 0
    exit true
end

function Length(self: String): NativeUInt = self.Length

end.