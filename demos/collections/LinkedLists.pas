uses Collections, System;

let x := NewLinkedList of Integer();

Append of Integer(x, 123);
Append of Integer(x, 456);

WriteLn('len of x: ' + IntToStr(Length of Integer(x)));

if Nth of Integer(x, 0) is Option.Some n then
    WriteLn('x[0]: ' + IntToStr(n));

if Nth of Integer(x, 1) is Option.Some n then
    WriteLn('x[1]: ' + IntToStr(n));