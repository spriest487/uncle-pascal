let x: Integer := if true then 123 else 312;
WriteLn(x);

WriteLn(begin
    let a: Integer := 543;
    let b: Integer := 345;

    if false then a else b;
end);