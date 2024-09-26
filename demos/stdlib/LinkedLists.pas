implementation

uses System, System.Collections;

initialization
    var x := NewLinkedList[Integer]();
    
    WriteLn('len of x: ' + x.LinkedListLength().IntToStr());

    x.LinkedListAppend(123);
    x.LinkedListAppend(456);
    
    WriteLn('len of x: ' + x.LinkedListLength().IntToStr());

    if x.LinkedListNth(0) is Option.Some n then
        WriteLn('x[0]: ' + n.IntToStr())
    else
        WriteLn('missing item 0! should be 123');
    
    if x.LinkedListNth(1) is Option.Some n then
        WriteLn('x[1]: ' + n.IntToStr())
    else
        WriteLn('missing item 0! should be 456');
end
