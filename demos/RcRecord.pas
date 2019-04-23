uses System;

type Box = class
    val: Integer;
end;

type Wrapper = record
    box: Box;
end;

type BigWrapper = record
    wrapper1: Wrapper;
    wrapper2: Wrapper
end;

function DoNothing(wrapper: Wrapper): Wrapper
begin
    wrapper
end;

begin
    let wrapper := Wrapper(box: Box(val: 123));

    DoNothing(wrapper);
end;

begin
    let wrapper := Wrapper(box: Box(val: 123));
    let wrapper2 := BigWrapper(wrapper1: wrapper; wrapper2: wrapper);
end;
