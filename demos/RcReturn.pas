uses System;

type Box = class
    val: Integer;
end;

function Id(box: Box): Box
begin
    box
end;

function StringId(s: String): String
begin
    s
end;

function New(): Box
begin
    Box(val:456)
end;
{
New();

let box := Box(val: 1);
Id(box);
}
var box2 := Box(val: 1);
box2 := New();

StringId('hello');
