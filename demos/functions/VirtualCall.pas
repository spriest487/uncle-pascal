uses System;

type NumberSource = interface
    function GetNumber(self: Self): Integer
end;

type NumberHolder = class
    number: Integer;
end;

function GetNumber of NumberSource(self: NumberHolder): Integer
begin
    self.number
end;

let source: NumberSource := NumberHolder(number: 123);

let number := source.GetNumber();
WriteLn(IntToStr(number));