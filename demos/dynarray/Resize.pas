uses System;

let x: array of Integer := [];
WriteLn(Length(x).ToString());

SetLength(x, 2, 0);
WriteLn(Length(x).ToString());