uses System;

System.WriteLn('Hello,' + ' world!');

var res: Result[Integer, String] := Result.Ok(123);

var thenAddOne := res.Then(lambda val: Result.Ok(val + 1));

match thenAddOne of
    Result.Ok val: WriteLn('OK: ' + val);
    else WriteLn('failed');
end;