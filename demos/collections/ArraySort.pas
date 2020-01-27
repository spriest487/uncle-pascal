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

QuickSort(foods);

let len := Length of String(foods);

for let i := 0 to len - 1 do begin
    WriteLn(foods[i]);
end;