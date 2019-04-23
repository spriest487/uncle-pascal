uses System;

function SayHello()
begin
    let msg := StringConcat('hello,', ' world!');
    WriteLn(msg);
end;

SayHello();
Uses.SayHello();

// todo: improve error message
// Uses.Uses.SayHello();