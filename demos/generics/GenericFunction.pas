uses System;

function Identity of T(id: T): T
begin
    id
end;

let one := Identity of Integer(1);
WriteLn('one is: ' + IntToStr(one));

let t := Identity of Boolean(true);
WriteLn('t is: ' + if t then 'True' else 'False');