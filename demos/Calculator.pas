uses System;

let addCommand: Byte := 43; // '+'
let subCommand: Byte := 45; // '-'
let mulCommand: Byte := 42; // '*'
let divCommand: Byte := 47; // '/'

WriteLn('Enter an expression e.g. ''2*3'', ''4/2'' or ''exit''.');

while true do begin
    let command := ReadLn();

    if command.CompareStr('exit') = 0 then break;

    var opIndex := -1;
    for let i := 0 to command.StringLen() - 1 do begin
        let c := command.StringCharAt(i);

        if c = mulCommand or c = divCommand or c = addCommand or c = subCommand then begin
            opIndex := i;
            break;
        end;
    end;

    if opIndex = -1 then begin
        WriteLn('invalid command: ''' + command + '''');
        continue;
    end;

    let op := command.StringCharAt(opIndex);
    let lhs := command.SubString(0, opIndex).StrToInt();
    let rhs := command.SubString(opIndex + 1, command.StringLen() - (opIndex + 1)).StrToInt();

    let result :=
        if op = addCommand then lhs + rhs
        else if op = subCommand then lhs - rhs
        else if op = divCommand then lhs / rhs
        else if op = mulCommand then lhs * rhs
        else begin
            WriteLn('invalid operator found for command ' + command);
            -1
        end;

    WriteLn(' => ' + result.IntToStr());
end;