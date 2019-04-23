//uses String;

function SayHello()
begin
    let msg := Strings.StringConcat('hello,', ' world!');
    WriteLn(msg);
end;

SayHello();
Uses.SayHello();

// todo: improve error message
Uses.Uses.SayHello();