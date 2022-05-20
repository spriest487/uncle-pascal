uses System;
{
var nums: array[3] of Integer := [1, 2, 3];
nums[0] := 999;

var num2 := @nums[1];
num2^ := 888;

for var i := 0 to 2 do begin
    WriteLn(IntToStr(nums[i]));
end;

var inferredNums := [1, 2, 3];

WriteLn('');

for var i := 0 to 2 do begin
    WriteLn(IntToStr(inferredNums[i]));
end;

WriteLn('');
}

var notEnoughNums: array[3] of Integer := [1, 999];
var y := notEnoughNums[4 as Integer];
{
for var i := 0 to 3 do begin
    WriteLn('notEnoughNums ' + i + ' = ' + notEnoughNums[i]);
end;}