implementation
uses System;

type I = interface
    function First(b: Integer);
    
    {
        not currently possible: interface methods always have an implicit self param in position 0
        so this would need explicitly static functions (eg `class method`) to work again
    }
    // class function Second(a: A; b: Self);

    function Both(b: Self);
end;

type A = class of I
    function First(b: Integer);
    // class function Second(a: A; b: Self);
    function Both(b: Self);
end;

function A.First(b: Integer);
begin
    WriteLn('first');
end;

{
function A.Second(a: Integer; b: C);
begin
    WriteLn('second');
end;
}

function A.Both(b: A);
begin
    WriteLn('both');
end;

initialization
    var a := A();
    I.First(a, 123);
    // I.Second(456, a);
    I.Both(a, a);
    
    a.First(123);
    a.Both(a);
end
