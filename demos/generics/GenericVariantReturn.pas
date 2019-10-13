uses System;

function ReturnSome of T(x: T): Option of T
begin
    Option.Some(x)
end

let someX := ReturnSome of Integer(123);

if someX is Option.Some x then
    WriteLn('someX is ' + IntToStr(x))
else
    WriteLn('error: x is not Some')