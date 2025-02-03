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
    
    for var food in foods do
        WriteLn(food);
    
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
    
    for var num in numbers do
        WriteLn(num.ToString);
end
