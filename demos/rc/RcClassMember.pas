uses System;

type Box = class
    val: Integer;
end;

type BigBox = class
    a: Box;
    b: Box;
end;

let boxes := BigBox(a: Box(val: 1); b: Box(val: 2));
WriteLn(IntToStr(boxes.a.val));
WriteLn(IntToStr(boxes.b.val));