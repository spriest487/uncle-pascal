implementation

uses System, System.Collections;

initialization
    var x: LinkedList[Integer] := LinkedList.Create();

    WriteLn('len of x: ' + x.Length());

    x.Add(123);
    x.Add(456);
    
    WriteLn('len of x: ' + x.Length());

    if x.TryGet(0) is Option.Some n then
        WriteLn('x[0]: ' + n)
    else
        WriteLn('missing item 0! should be 123');
    
    if x.TryGet(1) is Option.Some n then
        WriteLn('x[1]: ' + n)
    else
        WriteLn('missing item 0! should be 456');
end
