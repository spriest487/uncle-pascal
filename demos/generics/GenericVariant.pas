uses System;

type Option<T> = variant
    Some: T;
    None;
end;

let some: Option<String> := Option.Some('thing');

if some is Option.Some val then
    WriteLn('some is ' + val);

let none: Option<String> := Option.None();

if none is Option.Some val then
    WriteLn('oops, none had a value: ' + val)
else if none is Option.None then
    WriteLn('none is empty');