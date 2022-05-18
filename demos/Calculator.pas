unit Calculator;

implementation

uses System;

const addCommand: Byte = 43; // '+'
    subCommand: Byte = 45; // '-'
    mulCommand: Byte = 42; // '*'
    divCommand: Byte = 47; // '/'

initialization

WriteLn('Enter an expression e.g. ''2*3'', ''4/2'' or ''exit''.');

while true do begin
    var command := ReadLn().StringTrim();

    if command.CompareStr('exit') = 0 then break;

    var opIndex := -1;
    for var i := 0 to command.StringLen() - 1 do begin
        var c := command.StringCharAt(i);

        if c = mulCommand or c = divCommand or c = addCommand or c = subCommand then begin
            opIndex := i;
            break;
        end;
    end;

    if opIndex = -1 then begin
        WriteLn('invalid command: ''' + command + '''');
        continue;
    end;

    var op := command.StringCharAt(opIndex);
    var lhs := command.SubString(0, opIndex)
        .StringTrim()
        .StrToInt();
    var rhs := command.SubString(opIndex + 1, command.StringLen() - (opIndex + 1))
        .StringTrim()
        .StrToInt();

    var result := case op of
        addCommand: lhs + rhs;
        subCommand: lhs - rhs;
        divCommand: lhs div rhs;
        mulCommand: lhs * rhs;
        else begin
            WriteLn('invalid operator found for command ' + command);
            -1
        end;
    end;

    WriteLn(' => ' + result.IntToStr());
end;

end.