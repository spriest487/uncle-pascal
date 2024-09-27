unit System.Collections;
interface

uses System;

type Node[T] = class
    next: Option[Node[T]];
    val: T;
end;

type LinkedList[T] = class
    head: Option[Node[T]];
    
    function Length: Integer;
    function Nth(n: Integer): Option[T];
    function Append(item: T);
end;

function NewLinkedList[T](): LinkedList[T];

implementation

function GetNthNode[T](list: LinkedList[T]; n: Integer): Option[Node[T]];
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

function NewLinkedList[T](): LinkedList[T];
begin
    LinkedList(
        head: Option.None();
    )
end;

function LinkedList[T].Length: Integer;
begin
    if self.head is Option.Some head then 
    begin
        var current := head.next;
        var count := 0;
    
        while true do begin
            count := count + 1;
    
            if current is Option.Some node then 
            begin
                current := node.next;
            end
            else begin
                break;
            end;
        end;
    
        count;
    end
    else 0;
end;

function LinkedList[T].Nth(n: Integer): Option[T];
begin
    var nth := GetNthNode[T](self, n);

    var result: Option[T] := if nth is Option.Some node then
        Option.Some(node.val)
    else
        Option.None();

    result
end;

function LinkedList[T].Append(item: T);
begin
    if self.head is Option.Some head then begin
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
        self.head := Option.Some(Node(
            next: Option.None();
            val: item;
        ));
    end;
end;

end
