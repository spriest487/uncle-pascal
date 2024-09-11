implementation
uses System;

initialization
    var some: Option[String] := Option.Some('thing');
    
    if some is Option.Some val then
        WriteLn('some is ' + val);
    
    var none: Option[String] := Option.None();
    
    if none is Option.Some val then
        WriteLn('oops, none had a value: ' + val)
    else if none is Option.None then
        WriteLn('none is empty');
end
