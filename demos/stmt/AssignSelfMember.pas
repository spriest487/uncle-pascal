type A = class
    val: Integer;
end;

function B(self: A; newVal: Integer)
begin
    self.val := newVal;
end;

let a := A(val: 123);
a.B(456);

