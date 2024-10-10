implementation
uses System;

initialization
    var initial: Result[Integer, String] := Result.Ok(123);
    
    var multiplier := 2;
    
    var plusOne := initial.Then(
        function(val: Integer): Result[Integer, String]; 
        begin
            Result.Ok(val + 1)
        end
    );

    var finalResult := plusOne.Then(lambda val: Result.Ok(val * multiplier));
    
    match finalResult of
        Result.Ok val: WriteLn('OK: ' + val);
        else WriteLn('failed');
    end;
end
