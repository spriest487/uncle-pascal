uses System;

function PrintNth(xs: array[2] of String; n: Integer)
begin
    let first := xs[n];
    WriteLn(first);
end;

let x := ['Hello, ' + 'World', 'Hello, ' + 'Everyone'];

let y := x;

PrintNth(y, 0);
PrintNth(y, 1);
