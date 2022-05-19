uses System;

var msg := 'Hello, ';
var who := 'world';

var greeting := StringConcat(msg, who);
WriteLn(greeting);

WriteLn(msg + who);

WriteLn('Hello ' + 'again!');

var stringWithNewLine := 'Hello, World!'#10;
WriteLn(stringWithNewLine + '...and another line');