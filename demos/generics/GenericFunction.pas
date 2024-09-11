implementation
uses System;

function Identity[T](id: T): T;
begin
    id
end;

function Identity2[T](id: T): T;
begin
    Identity[T](id)
end;

initialization
    var two := Identity[Integer](2);
    
    var one := Identity2[Integer](1);
    WriteLn('one is: ' + IntToStr(one));
    
    var t := Identity2[Boolean](true);
    WriteLn('t is: ' + if t then 'True' else 'False');
end
