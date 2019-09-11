uses System;

type IntBox = class
    val: Integer;
end;

type Wrapper = record
    box: IntBox;
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
    let wrapped := IntBox(val: 123);
    let wrapper := Wrapper(box: wrapped);

    DoNothing(wrapper);
end;

begin
    let wrapper := Wrapper(box: IntBox(val: 123));
    let wrapper2 := BigWrapper(wrapper1: wrapper; wrapper2: wrapper);
end;
