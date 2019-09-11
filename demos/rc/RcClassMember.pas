uses System;

type BigBox = class
    a: Box of Integer;
    b: Box of Integer;
end;

let boxes := BigBox(a: NewBox of Integer(1); b: NewBox of Integer(2));

let a := boxes.a;
let aVal := Unbox of Integer(a);

{
let bVal := Unbox of Integer(boxes.b);

WriteLn('box contents:');
WriteLn(IntToStr(aVal));
WriteLn(IntToStr(bVal));
}