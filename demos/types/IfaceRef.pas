implementation
uses System;

type Animal = interface
    function Speak(self: Self; i: Integer);
end;

type Dog = class
end;

function Speak of Animal(self: Dog; i: Integer);
begin
    WriteLn('woof!')
end;

initialization
    var dog: Animal := Dog();
    
    dog.Speak(1);
end
