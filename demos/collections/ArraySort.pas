uses System, Arrays;

var foods: array of String := [
    'apple',
    'melon',
    'yogurt',
    'cookies',
    'treacle',
    'onion',
    'banana',
    'mustard',
    'lemon',
];

QuickSort(foods);

let len := Length of String(foods);

for let i := 0 to len - 1 do begin
    WriteLn(foods[i]);
end;