unit System;

interface

type
    ShortInt = Int8;
    SmallInt = Int16;
    Integer = Int32;

    Byte = UInt8;
    Word = UInt16;
    Cardinal = UInt32;

    String = class
        chars: ^Byte;
        len: Int32;
        
        function Compare(other: String): Integer;
        function ToString: String;
    end;

    Disposable = interface
        function Dispose();
    end;

function GetMem(count: Int32): ^Byte; external 'rt';
function FreeMem(mem: ^Byte); external 'rt';

function Write(line: String); external 'rt';
function WriteLn(line: String); external 'rt';
function ReadLn(): String; external 'rt';

function Int8ToStr(i: Int8): String; external 'rt';
function UInt8ToStr(i: UInt8): String; external 'rt';
function Int16ToStr(i: Int16): String; external 'rt';
function UInt16ToStr(i: UInt16): String; external 'rt';
function Int32ToStr(i: Int32): String; external 'rt';
function UInt32ToStr(i: UInt32): String; external 'rt';
function Int64ToStr(i: Int64): String; external 'rt';
function UInt64ToStr(i: UInt64): String; external 'rt';
function NativeIntToStr(i: NativeInt): String; external 'rt';
function NativeUIntToStr(i: NativeUInt): String; external 'rt';
function RealToStr(value: Real32): String; external 'rt';
function PointerToStr(value: Pointer): String; external 'rt';

function StrToInt(s: String): Int32; external 'rt';

function ArrayLengthInternal(arr: Pointer): Int32; external 'rt';

function ByteToStr(i: Byte): String;
function IntToStr(i: Integer): String;

type
    Box[T] = class
        value: T
    end;

    Option[T] = variant
        Some: T;
        None;
    end;

    Result[T, E] = variant
        Ok: T;
        Error: E;
    end;

    Comparable = interface
        function Compare(other: Self): Integer;
    end;

    Displayable = interface
        function ToString(): String;
    end;

function Unbox[T](b: Box[T]): T;
function NewBox[T](value: T): Box[T];

function Then[T, E](result: Result[T, E]; f: function(T): Result[T, E]): Result[T, E];

function IsWhiteSpace(char: Byte): Boolean;

function StringLen(s: String): Integer;
function StringConcat(a, b: String): String;
function StringFromBytes(bytes: ^Byte; len: Integer): String;
function StringLenNullTerminated(chars: ^Byte): Integer;
function SubString(s: String; from: Integer; len: Integer): String;
function StringCharAt(s: String; at: Integer): Byte;
function StringToBytes(s: String; bytes: ^Byte; len: Integer);
function CompareStr(a, b: String): Integer;
function StringTrim(s: String): String;

function Max[T](a, b: T): T where T is Comparable;
function Min[T](a, b: T): T where T is Comparable;

function ArraySetLengthInternal(arr: Any; len: Integer; defaultVal: Pointer): Any; external 'rt';

function Length[T](arr: array of T): Integer;
function SetLength[T](var arr: array of T; len: Integer; defaultVal: T);

implementation

function ByteToStr(i: Byte): String;
begin
    UInt8ToStr(i)
end;

function IntToStr(i: Integer): String;
begin
    Int32ToStr(i)
end;

function StringLen(s: String): Integer;
begin
    s.len
end;

function Then[T, E](result: Result[T, E]; f: function(T): Result[T, E]): Result[T, E];
begin
    match result of
        Result.Ok val: f(val);
        else result;
    end;
end;

function IsWhiteSpace(char: Byte): Boolean;
begin
    case char of
        9: true;
        10: true;
        12: true;
        13: true;
        32: true;
        133: true;
        160: true;
        else false;
    end;
end;

function StringConcat(a, b: String): String;
begin
    if a.len = 0 and b.len = 0 then
        exit String(chars: nil; len: 0);

    if a.len = 0 then exit b;
    if b.len = 0 then exit a;

    var len := a.len + b.len;

    var bytes := GetMem(len);

    for var i := 0 to a.len - 1 do
        bytes[i] := a.chars[i];

    for var i := 0 to b.len - 1 do
        bytes[a.len + i] := b.chars[i];

    String(
        chars: bytes;
        len: len
    );
end;

function StringFromBytes(bytes: ^Byte; len: Integer): String;
begin
    if len = 0 then
        String(chars: nil; len: 0)
    else begin
        var strBytes: ^Byte := GetMem(len);

        for var i: Integer := 0 to len - 1 do begin
            strBytes[i] := bytes[i];
        end;

        String(chars: strBytes; len: len)
    end
end;

function StringLenNullTerminated(chars: ^Byte): Integer;
begin
    if chars = nil then exit 0;

    var len := 0;
    while chars[len] <> $0 do
    begin
        len += 1;
    end;

    len
end;

function SubString(s: String; from: Integer; len: Integer): String;
begin
    if from < 0 then raise 'substring start index must be 0 or greater';
    if len < 0 then raise 'substring length must be 0 or greater';
    if from > s.len then raise 'substring start index must not exceed original string length';
    if (from + len) > s.len then raise 'substring length must not exceed original string length';

    if len = 0 then
        exit '';

    var buf := GetMem(len);
    for var i := 0 to len - 1 do begin
        buf[i] := s.chars[from + i];
    end;

    String(chars: buf; len: len)
end;

function StringCharAt(s: String; at: Integer): Byte;
begin
    if at < 0 or at >= s.len then
        raise 'invalid index ' + IntToStr(at) + ' in string of length ' + IntToStr(s.len);

    s.chars[at]
end;

function StringToBytes(s: String; bytes: ^Byte; len: Integer);
begin
    if len = 0 or bytes = nil then
        exit;

    var max := if len < s.len then
        len
    else
        s.len;

    // need to make a mutable copy of this pointer since we're going to modify its contents
    var outBytes := bytes;
    for var i := 0 to max - 1 do
    begin
        outBytes[i] := s.chars[i];
    end;
end;

function CompareStr(a, b: String): Integer;
begin
    if a.len = 0 and b.len = 0 then begin
        exit 0;
    end;

    var aPos := 0;
    var bPos := 0;

    var cmp: Integer := 0;
    while true do begin
        if aPos < a.len and bPos < b.len then begin
            var aChar := a.chars[aPos];
            var bChar := b.chars[bPos];
            cmp := if aChar > bChar then 1
                else if bChar > aChar then -1
                else 0;

            aPos += 1;
            bPos += 1;
        end
        else if aPos < a.len and aPos >= b.len then begin
            // a is longer than b
            cmp := 1;
        end
        else if bPos < b.len and bPos >= a.len then begin
            // b is longer than a
            cmp := -1;
        end
        else begin
            // out of range of both
            break;
        end;

        if cmp <> 0 then break;
    end;

    cmp
end;

function StringTrim(s: String): String;
begin
    if s.len = 0 then begin
        exit s;
    end;

    var startAt := 0;
    var endAt := s.len - 1;

    while startAt < endAt and s.StringCharAt(startAt).IsWhiteSpace() do
    begin
        startAt += 1;
    end;

    while endAt >= startAt and s.StringCharAt(endAt).IsWhiteSpace() do
    begin
        endAt -= 1;
    end;

    var len := (endAt + 1) - startAt;
    SubString(s, startAt, len)
end;

function Length[T](arr: array of T): Integer;
unsafe begin
    ArrayLengthInternal(arr)
end;

function Unbox[T](b: Box[T]): T;
begin
    b.value
end;

function NewBox[T](value: T): Box[T];
begin
    Box(value: value)
end;

function UInt8.Compare(other: UInt8): Integer;
begin
    if self > other then 1
    else if self < other then -1
    else 0
end;

function UInt16.Compare(other: UInt16): Integer;
begin
    if self > other then 1
    else if self < other then -1
    else 0
end;

function UInt32.Compare(other: UInt32): Integer;
begin
    if self > other then 1
    else if self < other then -1
    else 0
end;

function UInt64.Compare(other: UInt64): Integer;
begin
    if self > other then 1
    else if self < other then -1
    else 0
end;

function NativeUInt.Compare(other: NativeUInt): Integer;
begin
    if self > other then 1
    else if self < other then -1
    else 0
end;

function Int8.Compare(other: Int8): Integer;
begin
    if self > other then 1
    else if self < other then -1
    else 0
end;

function Int16.Compare(other: Int16): Integer;
begin
    if self > other then 1
    else if self < other then -1
    else 0
end;

function Int32.Compare(other: Int32): Integer;
begin
    self - other
end;

function Int64.Compare(other: Int64): Integer;
begin
    if self > other then 1
    else if self < other then -1
    else 0
end;

function NativeInt.Compare(other: NativeInt): Integer;
begin
    if self > other then 1
    else if self < other then -1
    else 0
end;

function Pointer.Compare(other: Pointer): Integer;
begin
    if self > other then 1
    else if self < other then -1
    else 0
end;

function Real32.Compare(other: Real32): Integer;
begin
    if self > other then 1
    else if self < other then -1
    else 0
end;

function Boolean.Compare(other: Boolean): Integer;
begin
    if self and not other then 1
    else if other and not self then -1
    else 0
end;

function String.Compare(other: String): Integer;
begin
    CompareStr(self, other)
end;

function Int8.ToString(): String;
begin
    Int8ToStr(self)
end;

function Byte.ToString(): String;
begin
    ByteToStr(self)
end;

function Int16.ToString(): String;
begin
    Int16ToStr(self)
end;

function UInt16.ToString(): String;
begin
    UInt16ToStr(self)
end;

function Integer.ToString(): String;
begin
    IntToStr(self)
end;

function UInt32.ToString(): String;
begin
    UInt32ToStr(self)
end;

function Int64.ToString(): String;
begin
    Int64ToStr(self)
end;

function UInt64.ToString(): String;
begin
    UInt64ToStr(self)
end;

function NativeInt.ToString(): String;
begin
    NativeIntToStr(self)
end;

function NativeUInt.ToString(): String;
begin
    NativeUIntToStr(self)
end;

function Boolean.ToString(): String;
begin
    if self then 'true' else 'false'
end;

function Real32.ToString(): String;
begin
    RealToStr(self)
end;

function Pointer.ToString(): String;
begin
    PointerToStr(self)
end;

function String.ToString(): String;
begin
    self
end;

function Max[T](a, b: T): T
where
    T is Comparable;
begin
    if a.Compare(b) > 0 then a else b
end;

function Min[T](a, b: T): T
where
    T is Comparable;
begin
    if a.Compare(b) < 0 then a else b
end;

function SetLength[T](var arr: array of T; len: Integer; defaultVal: T);
begin
    if arr.Length() = len then exit;

    // must put this in a mutable local variable to take its address (can't address immutable vars)
    var defaultValVar := defaultVal;

    unsafe begin
        var defaultValPtr: Pointer := @defaultValVar;

        arr := if ArraySetLengthInternal(arr, len, defaultValPtr) is array of T newArr
            then newArr
            else raise 'unreachable';
    end;
end;

end.
