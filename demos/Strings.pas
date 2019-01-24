function StringConcat(a, b: String): String
begin
    if a.len = 0 and b.len = 0 then
        String(chars: nil; len: 0)
    else if a.len = 0 then
        b
    else if b.len = 0 then
        a
    else begin
        let len: Integer := a.len + b.len;
        let bytes: ^Byte := GetMem(len);

        for let i: Integer := 0 to a.len do begin
            (bytes + i)^ := (a.chars + i)^;
        end;

        for let i: Integer := 0 to b.len do begin
            (bytes + a.len + i)^ := (b.chars + i)^;
        end;

        String(bytes: bytes; len: len)
    end
end;

let msg: String := 'Hello, world!';

WriteLn(msg);