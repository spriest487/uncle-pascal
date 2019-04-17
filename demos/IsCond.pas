uses System;

type Animal = interface
    function Speak(self: Self);
end;

type Cat = class end;

function Animal.Speak(self: Cat)
begin
    WriteLn('nya');
end;

type Dog = class end;

function Animal.Speak(self: Dog)
begin
    WriteLn('wan');
end;

function Fetch(self: Dog)
begin
    WriteLn('fetching a ball');
end;

let cat: Animal := Cat();
if cat is Cat then cat.Speak();

let dog: Animal := Dog();
if dog is not Cat then dog.Speak();

if dog is Dog d then Fetch(d);

