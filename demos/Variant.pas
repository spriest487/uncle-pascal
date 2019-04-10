uses System;

type MaybeInt = variant
    Some: Integer;
    None;
end;

function Display(label: String; i: MaybeInt)
begin
    case i of
        MaybeInt.Some(val): begin
            WriteLn(label + ' is ' + val);
        end;

        MaybeInt.None: begin
            WriteLn(label + ' is nothing');
        end;
    end
end;

let x := MaybeInt.Some(1);
Display(x);

let y := MaybeInt.None;
Display(y);