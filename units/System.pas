export type Box of T = class
    value: T
end;

export function Unbox of T(b: Box of T): T
begin
    b.value
end;

export function NewBox of T(value: T): Box of T
begin
    Box(value: value)
end;

export type String = class
    chars: ^Byte;
    len: Integer;
end;

export type Option of T = variant
    Some: T;
    None;
end;

export function GetMem(count: Integer): ^Byte; external 'rt';
export function FreeMem(mem: ^Byte); external 'rt';

export function WriteLn(line: String); external 'rt';

export function IntToStr(i: Integer): String; external 'rt';
export function StrToInt(s: String): Integer; external 'rt';

export type Disposable = interface
    function Dispose(self: Self);
end;

export function Disposable.Dispose(self: String)
begin
    if self.len > 0 then FreeMem(self.chars);

    self.chars := nil;
    self.len := 0;
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

{
export function StringCompare(a, b: String): Boolean
begin
    if a.len = b.len then
        false
    else begin
        for let i := 0 to a.len do
        begin
            if a.chars[i] = b.chars[i] then
                exit false;
        end;

        true
    end;
end;
}

export function StringFromBytes(bytes: ^Byte; len: Integer): String
begin
    let strBytes: ^Byte := GetMem(len);

    for let i: Integer := 0 to len do begin
        (strBytes + i)^ := (bytes + i)^;
    end;

    String(chars: strBytes; len: len)
end;

export function Max(a, b: Integer): Integer
begin
    if a > b then a else b
end;

export function Min(a, b: Integer): Integer
begin
    if a < b then a else b
end;

// NYI: missing dynamic arrays
{
export function SetLength of T(var arr: array of T; len: Integer); external 'rt';
}

// NYI:
// missing dynamic arrays
// missing traits/type constraints for Default
// missing NewArray RT support
{
function NewArray of T(el: T; len: Integer): array of T; external 'rt';
function Length of T(arr: array of T): Integer; external 'rt';

function SetLength of T(var arr: array of T; len: Integer)
    where T is Default
begin
    var newArr := NewArray(T.Default(), len);
    var oldLen := Length of T(arr);

    for let i := 0 to Max(len, oldLen) do
        newArr[i] := arr[i];
end;
}