implementation
uses System;

type Animal = interface
    function Say(self: Self);
end;

type Cat = class
    name: String;
end;

function Animal.Say(self: Cat);
begin
    WriteLn(self.name + ' says meow!');
end;

type Dog = class
    name: String;
end;

function Animal.Say(self: Dog);
begin
    WriteLn(self.name + ' says woof!');
end;

initialization
    var animal: Animal;
    
    animal := Cat(name: 'Jesse');
    animal.Say();
    
    animal := Dog(name: 'James');
    animal.Say();
end
