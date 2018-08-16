unit System

interface

constructor StringCreate: String
begin
    result.Chars := nil
    result.Length := 0
end

constructor StringFromBytes(bytes: ^System.Byte; len: System.Integer): String
begin
    result.Chars := GetMem(len)
    result.Length := len

    for let c := 0 to result.Length do
        ^(result.Chars + c) := ^(bytes + c)
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

        for let c := 0 to a.Length do
            ^(result.Chars + c) := ^(a.Chars + c);

        for let c := 0 to b.Length do
            ^(result.Chars + a.Length + c) := ^(b.Chars + c);
    end;
end

implementation

end.