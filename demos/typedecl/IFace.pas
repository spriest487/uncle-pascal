implementation
uses System;

type Speaker = interface
    function Speak(self: Self): String
end;

type Dog = class
    name: String;
end;

type Cat = class
    name: String;
end;

function Speaker.Speak(self: Dog): String;
begin
    'woof!'
end;

function Speaker.Speak(self: Cat): String;
begin
    'meow!'
end;

initialization
    WriteLn('Hello: ');
    
    var rex := Dog(name: 'Rex');
    WriteLn(Speaker.Speak(rex));
    
    var ginger := Cat(name: 'Ginger');
    WriteLn(ginger.Speak());
    WriteLn(rex.Speak());
end
