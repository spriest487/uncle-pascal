type String = class
    chars: ^Byte;
    len: Integer;
end;

function StringFromBytes(strBytes: ^Byte; len: Integer): String
begin
    let strBytes: ^Byte := GetMem(len);

    for let i: Integer := 0 to len do begin
        (strBytes + i)^ := (bytes + i)^;
    end;

    String(bytes: bytes; len: len)
end;

function StringConcat(a, b: String): String
begin
    if a.len = 0 and b.len = 0 then
        String(chars: nil; len: 0)
    else if a.len = 0 then
        b
    else if b.len = 0 then
        a
    else begin
        let bytes: ^Byte := GetMem(len);
        let len: Integer := a.len + b.len;

        for let i: Integer := 0 to a.len do begin
            (bytes + i)^ := (a.chars + i)^;
        end;

        for let i: Integer := 0 to b.len do begin
            (bytes + a.len + i)^ := (b.chars + i)^;
        end;

        String(bytes: bytes; len: len)
    end
end;
