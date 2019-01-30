function DoMaybe(cond: Boolean)
begin
    if cond then
        WriteLn('Doing it')
    else
        WriteLn('Not doing it')
end;


let x: Integer := if true then 123 else 312;
WriteLn(IntToStr(x));

WriteLn(begin
    let a: Integer := 543;
    let b: Integer := 345;
    let cond: Boolean := false;

    IntToStr(if cond then a else b)
end);

DoMaybe(true);
DoMaybe(false);