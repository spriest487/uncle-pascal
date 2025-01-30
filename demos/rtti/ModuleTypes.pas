unit ModuleTypes;
implementation

type
    A = class
    end;
    
    B = record
        val: Integer;
    end;

initialization
    var xs: array of Integer := [2];
    xs[0] := 1;
    
    // TODO: hack to fix builtin pointer array type not triggering dynarray codegen
    var pts: array of Pointer := [];

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
    
    var dType := TypeInfo.Find('array of System.Int32');
    WriteLn('found array of Integer? ' + dType.IsSome);
end.
