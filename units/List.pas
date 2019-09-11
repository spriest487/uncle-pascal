uses System;

type LinkedList of T = class
    next: Option of LinkedList of T;
    val: T;
end;

function Length of T(list: LinkedList of T): Integer
begin
    var current := list;
    var count := 0;

    while true do begin
        count := count + 1;

        if current.next is Option.Some next then begin
            current := next;
        end
        else
            break;
    end;

    count
end;

var x: LinkedList of Integer;

let l: LinkedList of Integer := LinkedList(
    val: 123;
    next: Option.Some(LinkedList(
        val: 456;
        next: Option.None();
    ));
);

if l.next is Option.Some next then begin
    let l2 := l;
    WriteLn('Here');
end;

let lLen := Length of Integer(l);
WriteLn(IntToStr(lLen))