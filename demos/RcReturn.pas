type
    Box = class
        val: Integer;
    end;

function Id(box: Box): Box
begin
    box
end

let box: Box := Box(val: 1);
let box2: Box := Id(box)
