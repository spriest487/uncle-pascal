implementation

type 
    MyScope = record
        name: String;
        
        constructor Begin(name: String);
        destructor End;
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

constructor MyScope.Begin(name: String);
begin
    WriteLn('creating MyScope ' + name);
    (name: name)
end;
    
destructor MyScope.End;
begin
    WriteLn('destroying MyScope ' + self.name);
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
    var rec1 := MyScope.Begin('One');

    var var1 := MyVariant.Create;
    
    var class1 := MyClass(name: 'Bob');
end.
