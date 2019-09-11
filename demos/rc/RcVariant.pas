uses System;

type Things = variant
    Box: Box of Integer;
    Text: String;
    None;
end;

let boxVal := NewBox of Integer(123);
var v := Things.Box(boxVal);
v := Things.Text('hello world');
v := Things.None();