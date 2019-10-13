uses System;

function InferResultTy(i: Integer): Option of Integer
begin
    if i > 1 then Option.Some(i) else Option.None()
end;

let x := InferResultTy(2);