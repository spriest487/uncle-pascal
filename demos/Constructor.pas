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

function PrintAnimal(name: String; animal: Animal) =
    WriteLn(name
        + ' is an animal with '
        + StringFromInt(animal.Ears) + ' ears and '
        + StringFromInt(animal.Legs) + ' legs.')

function PrintIceCream(name: String; iceCream: IceCream) =
    WriteLn('for ' + name
            + ' I am having '
            + StringFromInt(iceCream.Scoops) + ' scoops of '
            + iceCream.Flavor + '.')

var
    fish: Animal

begin
    let cat: Animal = (Ears: 2; Legs: 4)
    let var spider: Animal := (Legs: 7; Ears: cat.Ears - 2)
    spider.Legs := spider.Legs + 1

    fish := ()

    PrintAnimal('cat', cat)
    PrintAnimal('spider', spider)
    PrintAnimal('fish', fish)

    let mainCourse: IceCream = (
        Flavor: 'Chocolate';
        Scoops: 3;
    )

    PrintIceCream('main course', mainCourse)

    let dessert: IceCream = (
        Flavor: mainCourse.Flavor + ' and Strawberry'
        Scoops: 1
    )

    PrintIceCream('dessert', dessert)
end.