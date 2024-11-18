implementation

type
    Numbers = set of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    
initialization
    var evens: Numbers := [2, 4, 6, 8, 10];

    for var i := 1 to 10 do begin
        var inSet := i in evens;
    end;
end.
