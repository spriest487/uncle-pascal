export type Box[T] = class
    value: T
end;

export function Unbox[T](b: Box[T]): T
begin
    b.value
end;

export function NewBox[T](value: T): Box[T]
begin
    Box(value: value)
end;

export type String = class
    chars: ^Byte;
    len: Integer;
end;

export type Option[T] = variant
    Some: T;
    None;
end;

export function GetMem(count: Integer): ^Byte; external 'rt';
export function FreeMem(mem: ^Byte); external 'rt';

export function WriteLn(line: String); external 'rt';
export function ReadLn(): String; external 'rt';

export function IntToStr(i: Integer): String; external 'rt';
export function StrToInt(s: String): Integer; external 'rt';

function ArrayLengthInternal(arr: Pointer): Integer; external 'rt';

export type Disposable = interface
    function Dispose(self: Self);
end;

export function Disposable.Dispose(self: String)
begin
    if self.len > 0 then FreeMem(self.chars);

    self.chars := nil;
    self.len := 0;
end;

export function StringLen(s: String): Integer
begin
    s.len
end;

export function IsWhiteSpace(char: Byte): Boolean
begin
    char = 9
    or char = 10
    or char = 12
    or char = 13
    or char = 32
    or char = 133
    or char = 160
end;

export function StringConcat(a, b: String): String
begin
    if a.len = 0 and b.len = 0 then
        String(chars: nil; len: 0)
    else if a.len = 0 then b
    else if b.len = 0 then a
    else begin
        let len := a.len + b.len;

        let bytes := GetMem(len);

        for let i := 0 to a.len - 1 do
            (bytes + i)^ := (a.chars + i)^;

        for let i := 0 to b.len - 1 do
            (bytes + a.len + i)^ := (b.chars + i)^;

        String(chars: bytes; len: len);
    end
end;

export function StringFromBytes(bytes: ^Byte; len: Integer): String
begin
    if len = 0 then
        String(chars: nil; len: 0)
    else begin
        let strBytes: ^Byte := GetMem(len);

        for let i: Integer := 0 to len do begin
            (strBytes + i)^ := (bytes + i)^;
        end;

        String(chars: strBytes; len: len)
    end
end;

export function SubString(s: String; from: Integer; len: Integer): String
begin
    // todo: bounds check
    if len = 0 then
        ''
    else begin
        var buf := GetMem(len);
        for let i := 0 to len - 1 do begin
            (buf + i)^ := (s.chars + from + i)^;
        end;

        String(chars: buf; len: len)
    end
end;

export function StringCharAt(s: String; at: Integer): Byte
begin
    // todo: bounds check
    (s.chars + at)^
end;

export function CompareStr(a, b: String): Integer
begin
    if a.len = 0 and b.len = 0 then begin
        exit 0;
    end;

    var aPos := 0;
    var bPos := 0;

    var cmp: Integer := 0;
    while true do begin
        if aPos < a.len and bPos < b.len then begin
            let aChar := (a.chars + aPos)^;
            let bChar := (b.chars + bPos)^;
            cmp := if aChar > bChar then 1
                else if bChar > aChar then -1
                else 0;

            aPos := aPos + 1;
            bPos := bPos + 1;
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

export function StringTrim(s: String): String
begin
    if s.len = 0 then begin
        exit s;
    end;

    var startAt := 0;
    while s.StringCharAt(startAt).IsWhiteSpace() do
        startAt := startAt + 1;

    var endAt := s.len - 1;
    while endAt > startAt and s.StringCharAt(endAt).IsWhiteSpace() do
        endAt := endAt - 1;

    let len := (endAt + 1) - startAt;
    SubString(s, startAt, len)
end;

export function Length[T](arr: array of T): Integer
unsafe begin
    ArrayLengthInternal(arr)
end;

export type Comparable = interface
    function Compare(self: Self; other: Self): Integer;
end;

export function Max[T](a, b: T): T
where T is Comparable
begin
    if a.Compare(b) > 0 then a else b
end;

export function Min[T](a, b: T): T
where T is Comparable
begin
    if a.Compare(b) < 0 then a else b
end;

export function Comparable.Compare(self: String; other: String): Integer
begin
    CompareStr(self, other)
end;

export function Comparable.Compare(self: Integer; other: Integer): Integer
begin
    self - other
end;

export type Displayable = interface
    function ToString(self: Self): String;
end;

export function Displayable.ToString(self: Integer): String
begin
    IntToStr(self)
end;

export function Displayable.ToString(self: Boolean): String
begin
    if self then 'true' else 'false'
end;

export function Displayable.ToString(self: String): String
begin
    self
end;

function ArraySetLengthInternal(arr: Any; len: Integer): Any; external 'rt';

export function SetLength[T](var arr: array of T; len: Integer)
begin
    arr := if ArraySetLengthInternal(arr, len) is array of T newArr
        then newArr
        else arr; // unreachable
end;