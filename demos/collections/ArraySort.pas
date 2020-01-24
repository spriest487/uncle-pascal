uses System, Arrays;

var foods: array of String := [
    'mustard',
    'onion',
    'apple',
    'melon',
    'yogurt',
    'cookies',
    'treacle',
    'banana',
    'lemon',
];

QuickSort of String(foods);

for let i := 0 to Length of String(foods) - 1 do begin
    WriteLn(foods[i]);
end;

var numbers: array of Integer := [
    5,
    1,
    123,
    66,
    6,
    2,
    0,
];

QuickSort of Integer(numbers);

for let i := 0 to Length of Integer(numbers) - 1 do begin
    // this shouldn't compile!
    WriteLn(numbers[i]);
    //WriteLn(numbers[i].IntToStr());
end;