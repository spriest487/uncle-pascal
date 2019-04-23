uses System;

type Animal = interface
    function Say(self: Self);
end;

type Cat = class
end;

function Animal.Say(self: Cat)
begin
    WriteLn('nya!');
end;

type Dog = class
end;

function Animal.Say(self: Dog)
begin
    WriteLn('wan!');
end;

let animal1: Animal := Cat();
let animal2: Animal := Dog();

animal1.Say();
animal2.Say();

