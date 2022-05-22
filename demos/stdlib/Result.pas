unit ResultDemo;

interface

implementation
uses
    System;

initialization

var initial: Result[Integer, String] := Result.Ok(123);

var multiplier := 2;

var finalResult := initial
    .Then(lambda val: Result.Ok(val + 1))
    .Then(lambda val: Result.Ok(val * multiplier));

match finalResult of
    Result.Ok val: WriteLn('OK: ' + val);
    else WriteLn('failed');
end;

end.