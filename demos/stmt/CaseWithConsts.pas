uses System;

let x := 4;

case x of
    4: WriteLn('It''s four');
    5: WriteLn('It''s five')

    else begin
        WriteLn('It''s not five!');
        raise 'bad value'
    end
end;