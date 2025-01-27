unit ModuleTypes;
implementation

type
    A = class
    end;
    
    B = record
        val: Integer;
    end;

initialization
    var allTypes := TypeInfo.LoadedTypes;
    for var i := 0 to allTypes.Length - 1 do begin
        WriteLn('loaded type: ' + allTypes[i].Name);
    end;

    var aType := TypeInfo.Find('ModuleTypes.A');
    WriteLn('found A? ' + aType.IsSome);
    
    var bType := TypeInfo.Find('ModuleTypes.B');
    WriteLn('found B? ' + bType.IsSome);

    var cType := TypeInfo.Find('ModuleTypes.C');
    WriteLn('found C? ' + cType.IsSome);
end.
