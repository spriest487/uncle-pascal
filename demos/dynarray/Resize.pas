uses System;

let x: array of Integer := [1, 2, 3];
WriteLn(Length(x).ToString());

SetLength(x, 2);
WriteLn(Length(x).ToString());