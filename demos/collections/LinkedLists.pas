uses List, System;

let x: LinkedList of Integer := NewLinkedList of Integer();

Append of Integer(x, 123);
Append of Integer(x, 456);

WriteLn('len of x: ' + IntToStr(Length of Integer(x)));