interface

uses System.*

type
    RcObj = class
        Counter: ^Int32
    end

    Holder = record
        Obj: RcObj
    end

function CreateRcObj(flag: ^Int32): RcObj
function Disposable.Dispose(self: RcObj)

implementation

function CreateRcObj(counter: ^Int32): RcObj
begin
    ^counter := ^counter + 1
    result := (Counter: counter)
end

function Disposable.Dispose(self: RcObj)
begin
    WriteLn('disposed instance ' + StringFromInt(^self.Counter))
    ^self.Counter := ^self.Counter - 1
end

end