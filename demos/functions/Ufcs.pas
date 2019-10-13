uses System;

type Person = record
    name: String;
end;

function Greet(person: Person)
begin
    WriteLn('hello, ' + person.name);
end;

let world := Person(name: 'World');
world.Greet();