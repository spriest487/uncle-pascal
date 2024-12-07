implementation

type 
    MyClass = class
    end;
    MyBytes = set of Byte;

initialization
    var myClassInfo := typeinfo(MyClass);
    
    WriteLn('class is named ''' + myClassInfo.Name + '''');
    WriteLn('integer is named ''' + typeinfo(Integer).Name + '''');
    WriteLn('object is named ''' + typeinfo(Object).Name + '''');
    WriteLn('array is named ''' + typeinfo(array of MyClass).Name + '''');
    WriteLn('static array is named ''' + typeinfo(array[2] of MyClass).Name + '''');
    //WriteLn('set flags is named ''' + typeinfo(MyBytes).Name + '''');
end.
