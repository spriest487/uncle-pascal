implementation

type 
    MyRecord = record
        name: String;
        destructor Destroy;
    end;
    
    MyClass = class
    private
        name: String;
        destructor Destroy;
    end;
    
    MyVariant = variant
        First: Integer;
        Second: String;
        constructor Create;
        destructor Destroy;
    end;
    
destructor MyRecord.Destroy;
begin
    WriteLn('destroying MyRecord ' + self.name);
end;

destructor MyClass.Destroy;
begin
    WriteLn('destroying MyClass ' + self.name);
end;

constructor MyVariant.Create;
begin
    MyVariant.First(123);
end;

destructor MyVariant.Destroy;
begin
    match self of
        MyVariant.First num: WriteLn('destroying MyVariant.First: ' + num);
        MyVariant.Second text: WriteLn('destroying MyVariant.First: ' + text);
        else begin end;
    end;
end;

initialization
    var rec1 := MyRecord(name: 'One');
    rec1.Destroy;

    var var1 := MyVariant.Create;
    var1.Destroy;
end.
