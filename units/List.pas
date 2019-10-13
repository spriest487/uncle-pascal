uses System;

type Node of T = class
    next: Option of Node of T;
    val: T;
end;

export type LinkedList of T = class
    head: Option of Node of T;
end;

export function NewLinkedList of T(): LinkedList of T
begin
    LinkedList(
        head: Option.None();
    )
end;

export function Length of T(list: LinkedList of T): Integer
begin
    if list.head is Option.Some head then begin
        var current := head.next;
        var count := 0;

        while true do begin
            count := count + 1;

            if current is Option.Some node then begin
                current := node.next;
            end
            else
                break;
        end;

        count;
    end
    else begin
        0;
    end;
end;

function NthNode of T(list: LinkedList of T; n: Integer): Option of Node of T
begin
    if list.head is Option.Some head then begin
        if n = 0 then
            Option.Some(head)
        else begin
            var current := head;
            var tooShort := false;

            for let i := 0 to (n - 1) do begin
                if current.next is Option.Some node then begin
                    current := node;
                end
                else begin
                    tooShort := true;
                    break;
                end;
            end;

            if tooShort then
                Option.None()
            else
                Option.Some(current)
        end
    end
    else begin
        Option.None()
    end
end;

export function Nth of T(list: LinkedList of T; n: Integer): Option of T
begin
    if NthNode of T(list, n) is Option.Some node then
        Option.Some(node.val)
    else
        Option.None()
end;

export function Append of T(list: LinkedList of T; item: T)
begin
    if list.head is Option.Some head then begin
        var current := head;

        while true do begin
            if current.next is Option.Some next then
                current := next
            else begin
                current.next := Option.Some(Node(
                    next: Option.None();
                    val: item;
                ));
            end
        end
    end
    else begin
        list.head := Option.Some(Node(
            next: Option.None();
            val: item;
        ));
    end
end;

