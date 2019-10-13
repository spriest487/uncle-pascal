uses System;

let x := NewBox of Integer(246);
let y := x.Unbox();

WriteLn('y: ' + y.IntToStr());