uses System;

function X();
const
    A: Int32 = 1;
var
    B, C: Int32 = A;
const
    D: Boolean := False;
begin
    WriteLn(A.ToString());
    WriteLn(B.ToString());
    WriteLn(C.ToString());
    WriteLn(D.ToString());
end;

X();