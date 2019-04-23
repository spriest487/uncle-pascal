uses System;

type Box<Val> = class
    val: Val;
end;

let y: Box<String> := Box(val: 'test');

//let x := Box(val: 1);


//let z: Box<String> := Box(val: 123);