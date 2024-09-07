implementation
uses System;

type A = record
    value: String;
end;

initialization
    var as: array of A := [];
    
    var a := A(value: 'hello');
    SetLength(as, 1, a);
end;
