uses System;

System.WriteLn('Hello,' + ' world!');

var res: Result[Integer, String] := Result.Ok(123);

var thenAddOne := Then(res, function(val: Integer): Result[Integer, String];
    begin
        Result.Ok(val + 1);
    end);

match thenAddOne of
    Result.Ok val: WriteLn('OK: ' + val);
    else WriteLn('failed');
end;