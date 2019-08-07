uses System;

type Dog = class
    name: String;
end;

type Cat = class
end;

let animal: Any := Dog(name: 'Rover');

if animal is not Cat then
    WriteLn('not a cat');

if animal is Dog d then
    WriteLn(d.name);

// can't assign Integer to Any
// let a: Any := 1;

