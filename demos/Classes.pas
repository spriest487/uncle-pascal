type IntBox = class
    value: Integer;
    otherValue: Integer;
end;

type IntBoxHolder = class
    box: IntBox;
end;

function Value(box: IntBox): Integer
begin
    box.otherValue
end;

function GetBox(holder: IntBoxHolder): IntBox
begin
    holder.box
end;

let box: IntBox := IntBox(value: 1; otherValue: 2);
let holder: IntBoxHolder := IntBoxHolder(box: box);

WriteLn(IntToStr(holder.box.value));
WriteLn(IntToStr(Value(holder.box)));

WriteLn(IntToStr(Value(GetBox(holder)) + 1));