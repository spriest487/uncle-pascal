uses System;

type Person = class
end;

type Dog = class
end;

function Greet(person: Person)
begin
    WriteLn('hello!');
end;

function Greet(person: Person)
begin
    WriteLn('good dog!');
end;

let person := Person();
let dog := Dog();

Greet(person);
Greet(dog);