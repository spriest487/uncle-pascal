uses System;

{ can't construct without an instance of itself }
type ValidButImpossible = class
    self: ValidButImpossible;
    self2: Self;
end;

type Rec = class
    self: ^Rec;
    self2: ^Self;
end;

var uninit: ValidButImpossible;

let rec: Rec := Rec(self: nil; self2: nil);