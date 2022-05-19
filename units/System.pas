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
    end;

    Disposable = interface
        function Dispose(self: Self);
    end;

function GetMem(count: Int32): ^Byte; external 'rt';
function FreeMem(mem: ^Byte); external 'rt';

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

function StrToInt(s: String): Int32; external 'rt';

function ArrayLengthInternal(arr: Pointer): Int32; external 'rt';

function ByteToStr(i: Byte): String;
function IntToStr(i: Integer): String;

{$IFNDEF NO_STDLIB}

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
        function Compare(self: Self; other: Self): Integer;
    end;

    Displayable = interface
        function ToString(self: Self): String;
    end;

function Unbox[T](b: Box[T]): T;
function NewBox[T](value: T): Box[T];

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

{$ENDIF}

implementation

function Dispose of Disposable(self: String);
begin
    if self.len > 0 then
        FreeMem(self.chars);

    self.chars := nil;
    self.len := 0;
end;

function ByteToStr(i: Byte): String;
begin
    UInt8ToStr(i)
end;

function IntToStr(i: Integer): String;
begin
    Int32ToStr(i)
end;

{$IFNDEF NO_STDLIB}

function StringLen(s: String): Integer;
begin
    s.len
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
        raise 'invalid index: ' + IntToStr(at);

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
    while s.StringCharAt(startAt).IsWhiteSpace() do
        startAt += 1;

    var endAt := s.len - 1;
    while endAt > startAt and s.StringCharAt(endAt).IsWhiteSpace() do
        endAt -= 1;

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

function Compare of Comparable(self: String; other: String): Integer;
begin
    CompareStr(self, other)
end;

function Compare of Comparable(self: Integer; other: Integer): Integer;
begin
    self - other
end;

function ToString of Displayable(self: Int8): String;
begin
    Int8ToStr(self)
end;

function ToString of Displayable(self: Byte): String;
begin
    ByteToStr(self)
end;

function ToString of Displayable(self: Int16): String;
begin
    Int16ToStr(self)
end;

function ToString of Displayable(self: UInt16): String;
begin
    UInt16ToStr(self)
end;

function ToString of Displayable(self: Integer): String;
begin
    IntToStr(self)
end;

function ToString of Displayable(self: UInt32): String;
begin
    UInt32ToStr(self)
end;

function ToString of Displayable(self: Int64): String;
begin
    Int64ToStr(self)
end;

function ToString of Displayable(self: UInt64): String;
begin
    UInt64ToStr(self)
end;

function ToString of Displayable(self: NativeInt): String;
begin
    NativeIntToStr(self)
end;

function ToString of Displayable(self: NativeUInt): String;
begin
    NativeUIntToStr(self)
end;

function ToString of Displayable(self: Boolean): String;
begin
    if self then 'true' else 'false'
end;

function ToString of Displayable(self: String): String;
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

{$ENDIF}

end.