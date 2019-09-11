uses System;

type MaybeIntBox = variant
    Some: Box of Integer;
    None;
end;

type Outer = class
    maybeBox: MaybeIntBox;
end;

let maybeBox := MaybeIntBox.Some(
    NewBox of Integer(123)
);

let outer := Outer(
    maybeBox: maybeBox;
);

{
if outer.maybeBox is MaybeIntBox.Some box then begin
end;
}