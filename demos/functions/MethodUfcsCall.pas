implementation
uses System;

type Greeting = interface
    function Greet(self: Self);
end;

type HelloWorld = class
end;

function Greet of Greeting(self: HelloWorld);
begin
    WriteLn('Hello, world');
end;

function Greet of Greeting(self: String);
begin
    WriteLn('Hello, ' + self);
end;

initialization
    var hw := HelloWorld();
    
    hw.Greet();
    'Alice'.Greet();
end
