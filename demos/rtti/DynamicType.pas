implementation

type
    IAnimal = interface
    end;

    Cat = class of IAnimal
    end;
    
    Dog = class of IAnimal
    public
        function Bark;
    end;
    
function Dog.Bark;
begin
    WriteLn('woof!');
end;

initialization
    var animals: array of IAnimal := [
        Cat() as IAnimal,
        Dog() as IAnimal,
        Cat() as IAnimal,
    ];
    
    for var i := 0 to animals.Length - 1 do begin
        var animalType := TypeInfo.Get(animals[i]);
        WriteLn('animal ' + i + ' is a ' + animalType.Name);
        
        if animalType.FindMethod('Bark') is Option.Some method then
        unsafe begin
            method.Invoke(@animals[i] as Pointer, [], nil);
        end;
    end;
end.
