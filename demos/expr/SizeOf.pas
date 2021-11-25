uses System;

type SomeRecord = record
    f1: Integer;
    f2: Integer;
    f3: Integer;
end;

let intSize := sizeof(Integer);
WriteLn('int size: ' + intSize.ToString());

let recordSize := sizeof(SomeRecord);
WriteLn('SomeRecord size: ' + recordSize.ToString());

let strSize := sizeof(String);
WriteLn('string size: ' + strSize.ToString());

let arrayOf20IntsSize := sizeof(array[20] of Integer);
WriteLn('array of 20 ints size: ' + arrayOf20IntsSize.ToString());