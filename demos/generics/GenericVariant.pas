uses System;

type Option<T> = variant
    Some: T;
    None;
end;

let some: Option<String> := Option.Some('thing');
if something is Option.Some val then WriteLn(val);