uses System;

var nums: array[3] of Integer := [1, 2, 3];

nums[0] := 999;

let num2 := @nums[1];
num2^ := 888;

for let i := 0 to 2 do begin
    WriteLn(IntToStr(nums[i]));
end;

let inferredNums := [1, 2, 3];

for let i := 0 to 2 do begin
    WriteLn(IntToStr(inferredNums[i]));
end;