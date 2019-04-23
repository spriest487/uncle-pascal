uses System;

let msg := 'Hello, ';
let who := 'world';

let greeting := StringConcat(msg, who);
WriteLn(greeting);

WriteLn(msg + who);

WriteLn('Hello ' + 'again!');

