program RcNestedTest

uses RcTypes.*

type
    Outer = class
        Inner: RcObj
    end

begin
    let var counter := 0

    begin
        let outer: Outer = (
            Inner: CreateRcObj(@counter)
        )

        if counter = 0 then raise 'Inner should not be disposed'
    end

    if counter = 0 then WriteLn('Ok!') else raise 'Inner should be disposed'
end.