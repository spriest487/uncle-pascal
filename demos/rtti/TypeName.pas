implementation

type MyClass = class
end;

initialization
    var myClassInfo := typeinfo(MyClass);
    
    WriteLn('class is named ' + myClassInfo.Name);
end.
