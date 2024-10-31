implementation
uses 
    System,
    Arrays;

initialization
    var foods: array of String := [
        'mustard',
        'onion',
        'apple',
        'melon',
        'yogurt',
        'cookies',
        'treacle',
        'banana',
        'lemon'
    ];
    
    QuickSort(foods);
    
    for var i := 0 to Length(foods) - 1 do begin
        WriteLn(foods[i].ToString());
    end;
    
    var numbers: array of Integer := [
        5,
        1,
        123,
        66,
        6,
        2,
        0
    ];
    
    QuickSort(numbers);
    
    for var i := 0 to Length(numbers) - 1 do begin
        WriteLn(numbers[i].ToString());
    end;
end
