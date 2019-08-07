uses System;

if true then begin
    let a := 'true';

    // error: not defined
    // WriteLn(b);
end
else begin
    let b := 'false';
    WriteLn(b);

    // error: not defined
    // WriteLn(a);
end;

// error: not defined
// WriteLn(a);
// WriteLn(b);

if true then
    let a := 'true'
else
    let b := 'false';

// error: not defined
// WriteLn(a);
// WriteLn(b);