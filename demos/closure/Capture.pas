unit Capture;

implementation

uses System;

function GetAdder(a: Integer): function(Integer): Integer;
begin
    function(b: Integer): Integer;
    begin
        a + b
    end
end;

initialization

var add4 := GetAdder(4);
var z := add4(3);

WriteLn(z.ToString());

end.
