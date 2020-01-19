uses System;

type Animal = interface
    function Speak(self: Self; i: Integer);
end;

type Dog = class
end;

function Animal.Speak(self: Dog; i: Integer)
begin
    WriteLn('woof!')
end;

let dog: Animal := Dog();

dog.Speak(1);