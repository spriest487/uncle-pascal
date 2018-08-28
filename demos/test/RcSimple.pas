uses System.*
    RcTypes.*

begin
    let var counter := 0

    begin
        let obj = CreateRcObj(@counter)
        if counter = 0 then raise 'should not be disposed yet'
    end

    if counter = 0 then WriteLn('Ok!') else raise 'should be disposed'
end