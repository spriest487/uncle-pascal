implementation
uses System;

type TestBox[Val] = class
    val: Val;
end;

function UnwrapTestBox[T](box: TestBox[T]): T;
begin
    box.val
end;

initialization
    var box: TestBox[Integer] := TestBox(val: 1);
    var val := UnwrapTestBox[Integer](box);
    
    WriteLn(IntToStr(val));
end
