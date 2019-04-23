function StringConcat(a, b: System.String): System.String
begin
    if a.len = 0 and b.len = 0 then
        System.String(chars: nil; len: 0)
    else if a.len = 0 then b
    else if b.len = 0 then a
    else begin
        let len := a.len + b.len;

        let bytes := System.GetMem(len);

        for let i := 0 to a.len - 1 do
            (bytes + i)^ := (a.chars + i)^;

        for let i := 0 to b.len - 1 do
            (bytes + a.len + i)^ := (b.chars + i)^;

        System.String(chars: bytes; len: len);
    end
end;

let msg := 'Hello, ';
let who := 'world';

let greeting := StringConcat(msg, who);
System.WriteLn(greeting);

