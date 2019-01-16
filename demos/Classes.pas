type IntBox = class
    value: Integer;
end;

function Value(box: IntBox): Integer
begin
    box.value
end;

let box: IntBox := IntBox(value: 1);
