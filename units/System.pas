function GetMem(count: Integer): ^Byte;
function FreeMem(mem: ^Byte);

function WriteLn(line: String);

function IntToStr(i: Integer): String;

type String = class
    chars: ^Byte;
    len: Integer;
end;

type Disposable = interface
    function Dispose(self: Self);
end;

function Disposable.Dispose(self: String)
begin
    if self.len > 0 then FreeMem(self.chars);

    self.chars := nil;
    self.len := 0;
end;