implementation
uses System;

type Greetable = interface
    function Name(self: Self): String;
end;

type Person = record
    name: String;
end;

function Greetable.Name(self: Person): String;
begin
    self.name
end;

initialization
    var alice := Person(name: 'Alice');
    WriteLn('hello, ' + alice.Name());
end
