uses System;

type LinkedList of T = class
    next: Option of LinkedList of T;
    val: T;
end;

var x: LinkedList of Integer;

