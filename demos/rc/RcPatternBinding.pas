uses System;

type MaybeIntBox = variant
    Some: Box of Integer;
    None;
end;

type Outer = class
    maybeBox: MaybeIntBox;
end;

let outer := Outer(
    maybeBox: MaybeIntBox.Some(NewBox of Integer(123));
);

if outer.maybeBox is MaybeIntBox.Some box then begin
    WriteLn('yes');
end;