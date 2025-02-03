implementation

initialization
    // static array
    var numbers := [1, 2, 3];
    for var number in numbers do
        WriteLn(number.ToString);
    
    // dynarray    
    var letters := ['A', 'B', 'C'];
    for var letter in letters do
        WriteLn(letter);
    
    // string
    for var c in 'Hello, world' do
    begin
        var char := c as Byte;
        Write(StringFromBytes(@char, 1));
    end;
    WriteLn('!');
end
