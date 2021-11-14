uses System;

function X(); external 'ExternFuncs.dll';
function Y(val: Integer); external 'ExternFuncs.dll';
function Z(): Integer; external 'ExternFuncs.dll';

X();
Y(1);
let z := Z();

WriteLn(z.ToString());