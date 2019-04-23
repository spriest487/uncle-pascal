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

var animal: Animal;

animal := Cat(name: 'Blake');
animal.Say();

animal := Dog(name: 'Yang');
animal.Say();

