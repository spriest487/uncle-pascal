uses
    RcTypes.*

var
    counter: Int32 = 0

// function taking an rc param, so we can write an expression with an rc subvalue (the arg)
function RcFunc(obj, discarded: RcObj): RcObj = obj

function RcExitTest: RcObj
begin
    exit RcFunc(CreateRcObj(@counter), CreateRcObj(@counter))
end

begin
    begin
        let obj = RcExitTest()
        if counter <> 1 then raise 'object should have one reference'
    end

    if counter = 0 then WriteLn('Ok') else raise 'object should be disposed'
end