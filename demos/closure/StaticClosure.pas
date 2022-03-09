{ closure which captures nothing should create a static closure rather than allocating one instance per call }

uses System;

let x := function(): Integer
begin
    123
end;

let y := x();

WriteLn(IntToStr(y));