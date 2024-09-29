implementation

uses System, System.Collections;

initialization
    var x := NewLinkedList[Integer]();

    WriteLn('len of x: ' + x.Length());

    x.Append(123);
    x.Append(456);
    
    WriteLn('len of x: ' + x.Length());

    if x.Nth(0) is Option.Some n then
        WriteLn('x[0]: ' + n)
    else
        WriteLn('missing item 0! should be 123');
    
    if x.Nth(1) is Option.Some n then
        WriteLn('x[1]: ' + n)
    else
        WriteLn('missing item 0! should be 456');
end
