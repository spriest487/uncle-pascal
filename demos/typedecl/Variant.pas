uses System;

type MaybeInt = variant
    Some: Integer;
    None;
end;

let x := MaybeInt.Some(1);
let y := MaybeInt.None();