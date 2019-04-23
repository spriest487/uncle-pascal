uses System;

function RefHello(var s: String)
begin
    // ok: ref param must already be initialized
    WriteLn('RefHello says: ' + s);

    s := 'Hello, world!';
end;

function OutHello(out s: String)
begin
    // error: not initialized
    // WriteLn(s);

    s := 'Hello again!';
end;

{
// error: not mutable
let s := '';
RefHello(s);
OutHello(s);
}

{
var x := ['Salutations'];
// error: not a referencable expression (yet?)
RefHello(x[0]);
}

var x := '';

OutHello(x);
WriteLn('message is now: ' + x);


RefHello(x);
WriteLn('message is now: ' + x);

var y: String;
OutHello(y);
WriteLn('message is now: ' + y);