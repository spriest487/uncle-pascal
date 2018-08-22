program RcArrayTest

uses RcTypes.*

var
    counter: Int32 = 0
   
function LocalArray
begin
    let objs: array[1..3] of RcObj = [
        CreateRcObj(@counter)
        CreateRcObj(@counter)
        CreateRcObj(@counter)
    ]

    WriteLn('counter: ' + StringFromInt(counter))
    if counter <> 3 then raise 'All 3 instances should exist'
end

function ReturningArray(): array[1..3] of RcObj
begin
    let obj1 = CreateRcObj(@counter)
    let obj2 = CreateRcObj(@counter)
    let obj3 = CreateRcObj(@counter)

    result := [obj1, obj2, obj3]
end

begin
    LocalArray()
    if counter = 0 then WriteLn('Ok!')
    else raise 'All 3 instances should be disposed'

    ReturningArray()
    if counter = 0 then WriteLn('Ok!')
    else raise 'All 3 instances should be disposed'
end.