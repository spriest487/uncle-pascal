type Box[T] = class
    val: T;
end;

function BoxUp[T](val: T): Box[T]
begin
    Box(val: val)
end;

let i := 1;
let iBox: Box[Integer] := BoxUp(i);

let i2: Integer := iBox.val;

System.WriteLn(IntToStr(i2));

let s1 := 'test';
let sBox: Box[System.String] := BoxUp(s1);
let s2 := sBox.val;

System.WriteLn(s2);