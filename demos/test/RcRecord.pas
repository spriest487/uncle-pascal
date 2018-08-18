program RcRecordTest

uses System.*

type
    RcObj = class
        Flag: ^Boolean
    end

    Holder = record
        Obj: RcObj
    end

function Disposable.Dispose(self: RcObj)
begin
    WriteLn('disposed')
    ^self.Flag := true
end

begin
    let var flag := false

    begin
        let holder: Holder = (
            Obj: (Flag: @flag)
        )

        if flag then raise 'should not be disposed yet'
    end

    if flag then WriteLn('Ok!') else raise 'should be disposed'
end.