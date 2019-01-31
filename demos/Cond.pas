function Maybe(cond: Boolean): Integer
begin
    if cond then
        1
    else
        2
end;

let x: Integer := if true then 123 else 312;
WriteLn(IntToStr(x));

let z := if false then 'first'
    else if false then 'second'
    else 'third';

WriteLn(z);

WriteLn(begin
    let a: Integer := 543;
    let b: Integer := 345;
    let cond: Boolean := false;

    IntToStr(if cond then a else b)
end);

WriteLn(IntToStr(Maybe(true)));
WriteLn(IntToStr(Maybe(false)));