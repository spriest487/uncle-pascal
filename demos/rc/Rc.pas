type Box = class
    val1: Integer;
    val2: Boolean;
end;

type BoxBox = class
    box: Box;
end;

function DoNothing(bb: BoxBox)
begin
end;

let box1 := Box(val1: 1; val2: true);

let boxbox := BoxBox(box: box1);
DoNothing(boxbox);

