uses System;

type Circle = record
    radius: Integer;
end;

let x := Circle(radius: 3);

let radius: Integer := x.radius;
if radius > 3 then WriteLn('too big');

WriteLn(IntToStr(radius));