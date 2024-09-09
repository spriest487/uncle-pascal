implementation
uses System;

type A = class
end;

initialization
    var x := 1;
    var y := A();
    
    WriteLn('x is Any: ' + (if x is Any then true else false).ToString());
    WriteLn('y is Any: ' + (if y is Any then true else false).ToString());
end
