uses System;

type Animal = interface
    function Say(self: Self);
end;

type Cat = class
    name: String;
end;

function Animal.Say(self: Cat)
begin
    WriteLn(self.name + ' says nya!');
end;

type Dog = class
    name: String;
end;

function Animal.Say(self: Dog)
begin
    WriteLn(self.name + ' says wan!');
end;

let animal1: Animal := Cat(name: 'Blake');
let animal2: Animal := Dog(name: 'Yang');

animal1.Say();
animal2.Say();

