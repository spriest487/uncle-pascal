type
    IntBox = class
        value: Integer;
        otherValue: Integer;
    end;

    IntBoxHolder = class
        box: IntBox;
    end

function Value(box: IntBox): Integer
begin
    box.otherValue
end;

let box: IntBox := IntBox(value: 1; otherValue: 2);
let holder: IntBoxHolder := IntBoxHolder(box: box);
WriteLn(Value(holder.box));