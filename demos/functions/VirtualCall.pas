implementation
uses System;

type NumberSource = interface
    function GetNumber(self: Self): Integer
end;

type NumberHolder = class
    number: Integer;
end;

function GetNumber of NumberSource(self: NumberHolder): Integer;
begin
    self.number
end;

initialization
    var source: NumberSource := NumberHolder(number: 123);
    
    var number := source.GetNumber();
    WriteLn(IntToStr(number));
end
