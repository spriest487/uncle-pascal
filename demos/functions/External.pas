uses System;

function X(); external 'ExternFuncs.dll';
function Y(val: Integer); external 'ExternFuncs.dll';
function Z(): Integer; external 'ExternFuncs.dll';
function ReturnsIntPtr(): ^Integer; external 'ExternFuncs.dll';
function PrintIntPtr(ptr: ^Integer); external 'ExternFuncs.dll';

X();
Y(1);
let z := Z();

WriteLn(z.ToString());

begin
    let intPtr := ReturnsIntPtr();
    PrintIntPtr(intPtr);

    WriteLn('printing value of intPtr^: ' + intPtr^.ToString());
end;

// cast the hard way
{unsafe begin
    var intBytes: Pointer := GetMem(sizeof(Integer));
    var intPtr: ^Integer := intBytes;
    intPtr^ := 999888777;
    PrintIntPtr(intPtr);
end;
}