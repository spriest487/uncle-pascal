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
    
    function Get(n: Integer): T;
    function TryGet(n: Integer): Option[T];
    
    function Set(n: Integer; value: T);
    
    function Remove(n: Integer);
    
    function Append(item: T);
    
    function Clear;
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

function LinkedList[T].Get(n: Integer): T;
begin
    match self.TryGet(n) of
        Option.Some item: item;
        Option.None: raise 'index out of range: ' + n.ToString();
    end;
end;

function LinkedList[T].TryGet(n: Integer): Option[T];
begin
    var nth := GetNthNode[T](self, n);

    var result: Option[T] := if nth is Option.Some node then
        Option.Some(node.val)
    else
        Option.None();

    result
end;

function LinkedList[T].Set(n: Integer; value: T);
begin
    var nth := GetNthNode[T](self, n);

    if nth is Option.Some node then
        node.val := value
    else
        raise 'index out of range: ' + n.ToString();
end;

function LinkedList[T].Remove(n: Integer);
begin
    if n = 0 then
    begin
        match self.head of
            Option.Some head:
                self.head := head.next;
                
            Option.None:
                raise 'index out of range: 0'; 
        end; 
    end
    else
        match GetNthNode[T](self, n - 1) of
            Option.Some parent: 
                match parent.next of
                    Option.Some removed:
                        parent.next := removed.next;
                    
                    Option.None:
                        raise 'index out of range: ' + n.ToString();    
                
            Option.None:
                raise 'index out of range: ' + n.ToString();
        end;
    end;
end;

function LinkedList[T].Clear;
begin
    self.head := Option.None();
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
