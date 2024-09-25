implementation
uses System;

type NumberSource = interface
    function GetNumber: Integer
end;

type NumberHolder = class of NumberSource
    number: Integer;
    
    function GetNumber(): Integer;
end;

function NumberHolder.GetNumber(): Integer;
begin
    self.number
end;

initialization
    var source: NumberSource := NumberHolder(number: 123);
    
    var number := source.GetNumber();
    WriteLn(IntToStr(number));
end
