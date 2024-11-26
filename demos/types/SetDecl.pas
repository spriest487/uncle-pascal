implementation

type
    Digits = set of 1..10;
    {AllBytes = set of Byte;
    AllInt8s = set of Int8;
    AllInts = set of Integer;
    }

initialization
    var evens: Digits := [2, 4, 6, 8, 10];
    var odds: Digits := [1, 3, 5, 7, 9];

end.
