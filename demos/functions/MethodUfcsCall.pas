uses System;

type Greeting = interface
    function Greet(self: Self);
end;

type HelloWorld = class
end;

function Greeting.Greet(self: HelloWorld)
begin
    WriteLn('Hello, world');
end;

function Greeting.Greet(self: String)
begin
    WriteLn('Hello, ' + self);
end;

let hw := HelloWorld();

hw.Greet();
'Alice'.Greet();