uses System;

function DoNothing()
begin
end;

function DoSomething()
begin
    DoNothing()
end;

begin
    let x: Integer := 1;
    let y: Integer := 2;
    WriteLn('456');

    DoNothing();
end;

DoSomething();

WriteLn(IntToStr(123))