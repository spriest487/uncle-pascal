implementation

type
    MyClass = class
    published
        function MyMethod1: String;
        function MyMethod2(i: Integer);
    end;
    
function MyClass.MyMethod1: String;
begin
    'Hello world!';
end;

function MyClass.MyMethod2(i: Integer);
begin
end;

initialization
    var myClassInfo := typeinfo(MyClass);
    var methods := myClassInfo.Methods;
    for var i := 0 to Length(methods) - 1 do begin
        WriteLn(myClassInfo.Name + ' has method ' + methods[i].Name);
    end; 
end
