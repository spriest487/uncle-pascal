unit AnyRef;

implementation

uses System;

type Dog = class
    name: String;
end;

type Cat = class
end;

initialization

var animal: Any := Dog(name: 'Rover');

if animal is not Cat then
    WriteLn('not a cat');

if animal is Dog d then
    WriteLn(d.name);

// can't assign Integer to Any
// var a: Any := 1;

end.
