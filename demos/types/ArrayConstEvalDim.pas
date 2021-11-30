uses System;

const ARRAY_DIM: Integer = 3;

let x: array[ARRAY_DIM] of Integer := [1, 2, 3];

// x[2].ToString() parses wrong!
WriteLn('element 2: ' + (x[2]).ToString());
WriteLn('element 3: ' + (x[3]).ToString());