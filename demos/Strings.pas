function StringConcat(a, b: String): String
begin
    let aLen := a.len;
    let bLen := b.len;

    if aLen = 0 and bLen = 0 then String(chars: nil; len: 0)
    else if aLen = 0 then b
    else if bLen = 0 then a
    else begin
        let len := a.len + b.len;
        let bytes := GetMem(len);

        for let i := 0 to a.len - 1 do
            (bytes + i)^ := (a.chars + i)^;

        for let i := 0 to b.len - 1 do
            (bytes + a.len + i)^ := (b.chars + i)^;

        String(chars: bytes; len: len)
    end
end;

let msg := 'Hello, ';
let who := 'world';

let greeting := StringConcat(msg, who);
WriteLn(greeting);

