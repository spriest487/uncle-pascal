program ConstructorDemo

uses
    System.*

type
    Animal = record
        Ears: Int32
        Legs: Int32
    end

    IceCream = class
        Scoops: Int32
        Flavor: String
    end

function PrintAnimal(name: String; animal: Animal)
begin
    WriteLn(name
        + ' is an animal with '
        + StringFromInt(animal.Ears) + ' ears and '
        + StringFromInt(animal.Legs) + ' legs.')
end

function PrintIceCream(name: String; iceCream: IceCream)
begin
    WriteLn('for ' + name
            + ' I am having '
            + StringFromInt(iceCream.Scoops) + ' scoops of '
            + iceCream.Flavor + '.')
end

var
    cat, spider, fish: Animal
    mainCourse, dessert: IceCream

begin
    cat := (Ears: 2; Legs: 4)
    spider := (Legs: 8)
    fish := ()

    PrintAnimal('cat', cat)
    PrintAnimal('spider', spider)
    PrintAnimal('fish', fish)

    mainCourse := (
        Flavor: 'Chocolate';
        Scoops: 3;
    )

    PrintIceCream('main course', mainCourse)

    dessert := (
        Flavor: mainCourse.Flavor + ' and Strawberry'
        Scoops: 1
    )

    PrintIceCream('dessert', dessert)
end.