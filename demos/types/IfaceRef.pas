uses System;

type Animal = interface
    function Speak(self: Self);
end;

type Dog = class
end;

function Animal.Speak(self: Dog)
begin
    WriteLn('woof!')
end;

let dog: Animal := Dog();

dog.Speak();