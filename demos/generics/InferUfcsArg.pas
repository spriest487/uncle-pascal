uses System;

function Identity of T(t: T): T
begin
    t
end;

let x := 1;
let y := x.Identity();