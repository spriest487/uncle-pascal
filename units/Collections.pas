interface

uses System;

type Node[T] = class
    next: Option[Node[T]];
    val: T;
end;

type LinkedList[T] = class
    head: Option[Node[T]];
end;

function NewLinkedList[T](): LinkedList[T];
function LinkedListLength[T](list: LinkedList[T]): Integer;
function LinkedListNth[T](list: LinkedList[T]; n: Integer): Option[T];
function LinkedListAppend[T](list: LinkedList[T]; item: T);

implementation

function NewLinkedList[T](): LinkedList[T];
begin
    LinkedList(
        head: Option.None();
    )
end;

function LinkedListLength[T](list: LinkedList[T]): Integer;
begin
    if list.head is Option.Some head then begin
        var current := head.next;
        var count := 0;

        while true do begin
            count := count + 1;

            if current is Option.Some node then begin
                current := node.next;
            end
            else begin
                break;
            end;
        end;

        count;
    end
    else begin
        0;
    end;
end;

function NthNode[T](list: LinkedList[T]; n: Integer): Option[Node[T]];
begin
    if list.head is Option.Some head then begin
        if n = 0 then
            Option.Some(head)
        else begin
            var current := head;
            var tooShort := false;

            for var i := 0 to (n - 1) do begin
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

function LinkedListNth[T](list: LinkedList[T]; n: Integer): Option[T];
begin
    var nth := NthNode[T](list, n);

    var result: Option[T] := if nth is Option.Some node then
        Option.Some(node.val)
    else
        Option.None();

    result
end;

function LinkedListAppend[T](list: LinkedList[T]; item: T);
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
                break;
            end
        end
    end
    else begin
        list.head := Option.Some(Node(
            next: Option.None();
            val: item;
        ));
    end;
end;

end
