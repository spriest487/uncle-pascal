uses System;

let apple := 'Apple';
let orange := 'Orange';

let fruit := 'Orange';

case fruit of
    apple: WriteLn('It''s an apple!!')

    else begin
        WriteLn('it''s ' + fruit);
    end
end;