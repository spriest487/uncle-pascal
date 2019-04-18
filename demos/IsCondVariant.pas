uses System;

type MaybeInt = variant
    Some: Integer;
    None;
end;

let x := MaybeInt.Some(1);
if x is MaybeInt.Some val then
    WriteLn('x is Some ' + IntToStr(val));

if x is not MaybeInt.None then
    WriteLn('x is not None');

let y := MaybeInt.None();
if y is MaybeInt.None then
    WriteLn('y is None');

if y is not MaybeInt.Some then
    WriteLn('y is not Some');