implementation
uses System;

type Greetable = interface
    function Name(self: Self): String;
end;

type Person = record
    name: String;
end;

function Name of Greetable(self: Person): String;
begin
    self.name
end;

type Cat = class
end;

function Name of Greetable(self: Cat): String;
begin
    'kitty'
end;

function Greet[T](t: T) 
where 
    T is Greetable;
begin
    WriteLn('hello, ' + t.Name());
    WriteLn('hello again, ' + Greetable.Name(t));
end;

initialization
    var alice := Person(name: 'Alice');
    Greet(alice);
    
    var kitty := Cat();
    Greet(kitty);

    //Greet('nobody');
end
