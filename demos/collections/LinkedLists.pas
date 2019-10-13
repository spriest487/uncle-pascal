uses System, Collections;

let x := NewLinkedList of Integer();

WriteLn('len of x: ' + x.Length().IntToStr());

x.Append(123);
x.Append(456);

WriteLn('len of x: ' + x.Length().IntToStr());

if x.Nth(0) is Option.Some n then
    WriteLn('x[0]: ' + n.IntToStr());

if x.Nth(1) is Option.Some n then
    WriteLn('x[1]: ' + n.IntToStr());
