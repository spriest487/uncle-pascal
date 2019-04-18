uses System;

type MaybeInt = variant
    Some: Integer;
    None;
end;

let x := MaybeInt.Some(1);
if x is MaybeInt.Some val then
    WriteLn('value of x is ' + IntToStr(val));

let y := MaybeInt.None();
if y is MaybeInt.None then
    WriteLn('y is None');