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

function Speaker.Speak(self: Dog): String
begin
    'wan!'
end;

function Speaker.Speak(self: Cat): String
begin
    'yang!'
end;

WriteLn('Hello: ');

let rex := Dog(name: 'Rex');
WriteLn(Speaker.Speak(rex));

let blake := Cat(name: 'Blake');
WriteLn(blake.Speak());
WriteLn(rex.Speak());