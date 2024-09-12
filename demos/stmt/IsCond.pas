implementation
uses System;

type Animal = interface
    function Speak(self: Self);
end;

type Cat = class end;

function Speak of Animal(self: Cat);
begin
    WriteLn('nya');
end;

type Dog = class end;

function Speak of Animal(self: Dog);
begin
    WriteLn('wan');
end;

function Fetch(self: Dog);
begin
    WriteLn('fetching a ball');
end;

initialization
    var cat: Animal := Cat();
    if cat is Cat then cat.Speak();
    
    var dog: Animal := Dog();
    if dog is not Cat then dog.Speak();
    
    if dog is Dog d then Fetch(d);
    
    if dog is Animal then WriteLn('yes, a dog is an animal');
    if dog is not Disposable then WriteLn('a dog is not just for christmas');
end
