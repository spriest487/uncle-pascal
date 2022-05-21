unit ResultDemo;

interface

implementation
uses
    System;

initialization

var initial: Result[Integer, String] := Result.Ok(123);

var finalResult := initial
    .Then(function(val: Integer): Result[Integer, String];
        begin
            Result.Ok(val + 1);
        end
    ).Then(function(val: Integer): Result[Integer, String];
        begin
            Result.Ok(val * 2);
        end
    );

match finalResult of
    Result.Ok val: WriteLn('OK: ' + val);
    else WriteLn('failed');
end;

end.