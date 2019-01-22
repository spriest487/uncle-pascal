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

begin
   let box: IntBox := IntBox(value: 1; otherValue: 2);
   let holder: IntBoxHolder := IntBoxHolder(box: box);

    //let greeting: String := 'Hello, world!';

    //WriteLn(greeting);
end