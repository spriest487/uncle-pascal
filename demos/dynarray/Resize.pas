implementation
uses System;

initialization
    var x: array of Integer := [];
    WriteLn(Length(x).ToString());
    
    SetLength(x, 2, 0);
    WriteLn(Length(x).ToString());
    
    var strings: array of Box[Integer] := [NewBox(1), NewBox(2)];
    SetLength(strings, 0, NewBox(0));
end
