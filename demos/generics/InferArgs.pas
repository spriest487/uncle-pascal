function GenFunc of T(t: T)
begin
    if t is System.String s then
        System.WriteLn('called GenFunc with a string')
    else
        System.WriteLn('called GenFunc with something else');
end;

GenFunc(1);
GenFunc('a');
GenFunc(true);