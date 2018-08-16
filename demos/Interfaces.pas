program InterfacesDemo

uses System.*

type Animal = interface
    function SayName(self: Self)
    function CountLegs(self: Self): Int32
end

type Cat = class
    Name: String
end

type Bird = class
    Name: String
end

function Animal.SayName(cat: Cat)
    WriteLn('nya... cats can''t talk')

function Animal.CountLegs(cat: Cat): Int32
    result := 4

function Animal.SayName(bird: Bird)
    WriteLn(bird.Name)

function Animal.CountLegs(bird: Bird): Int32
    result := 2

begin
    let penny: Cat = (Name: 'Penny')
    let polly: Bird = (Name: 'Polly')

    penny.SayName()
    //Animal.SayName(polly)

    let animal1: Animal = penny
    let animal2: Animal = polly

    let totalLegs = animal1.CountLegs() + animal2.CountLegs()
    WriteLn('total legs: ' + StringFromInt(totalLegs))
end.