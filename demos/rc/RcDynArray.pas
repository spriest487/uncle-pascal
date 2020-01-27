uses System;

function PrintNth(xs: array of String; n: Integer)
begin
    let x := xs[n];
    WriteLn(x);
end;

let arr: array of String := ['1', '2', '3'];
PrintNth(arr, 0);

let arr2 := arr;
PrintNth(arr2, 2);
