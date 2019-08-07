uses System;

function Identity of T(id: T): T
begin
    id
end;

function Identity2 of T(id: T): T
begin
    Identity of T(id)
end;

let two := Identity of Integer(2);

let one := Identity2 of Integer(1);
WriteLn('one is: ' + IntToStr(one));

let t := Identity2 of Boolean(true);
WriteLn('t is: ' + if t then 'True' else 'False');
