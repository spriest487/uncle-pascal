uses System;

type A = class
end;

let as: array of A := [A(), A()];
for let i := 0 to Length(as) - 1 do begin
    let a := as[i];
end;