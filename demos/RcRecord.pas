type
    Box = class
        val: Integer;
    end;

    Wrapper = record
        box: Box;
    end;

function DoNothing(wrapper: Wrapper): Wrapper
begin
    wrapper
end;

let wrapper: Wrapper := Wrapper(box: Box(val: 123));

DoNothing(wrapper);