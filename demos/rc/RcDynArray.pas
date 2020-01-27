uses System;

function PrintNth(xs: array of Integer; n: Integer)
begin
    let x := xs[n];
    WriteLn(x.IntToStr());
end;

let arr: array of Integer := [1, 2, 3];
PrintNth(arr, 0);

let arr2 := arr;
PrintNth(arr2, 2);