program RcArrayTest

uses RcTypes.*

var
    counter: Int32 = 0
   
function Test
begin
    let objs: array[1..3] of RcObj = [
        CreateRcObj(@counter)
        CreateRcObj(@counter)
        CreateRcObj(@counter)
    ]

    WriteLn('counter: ' + StringFromInt(counter))
    if counter <> 3 then raise 'All 3 instances should exist'
end

begin
    Test()
    
    WriteLn('counter: ' + StringFromInt(counter))
    
    if counter = 0 then 
        WriteLn('Ok!') 
    else 
        raise 'All 3 instances should be disposed'
end.