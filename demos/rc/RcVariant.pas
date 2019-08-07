uses System;

type Box = class
    value: Integer;
end;

type Things = variant
    Box: Box;
    Text: String;
    None;
end;

var v := Things.Box(Box(value: 123));
v := Things.Text('hello world');
v := Things.None();