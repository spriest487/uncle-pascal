uses System.*

type
    Fruit = (Apple, Orange, Pear, Banana, Cherry, Plum)
    FruitSalad = Apple..Banana
    Clothes = set of (Socks, Shirt, Hat)

var
    favoriteFruit: Fruit
    clothes: Clothes
begin
    favoriteFruit := [Apple, Pear]
    let wearing = [Socks..Hat]

    if Apple in favoriteFruit then
        WriteLn('i like apples')

    //clothes := [Socks, Hat]
end