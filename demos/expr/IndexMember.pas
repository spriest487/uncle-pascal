type S = record
    values: array[3] of Integer;
end;

var s := S(values: [4, 4, 4]);

s.values[0] := 1;
s.values[1] := 2;
s.values[2] := 3;