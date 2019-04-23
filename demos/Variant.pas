type
    Option of T = variant
        some: T,
        none: Nothing,
    end;

let x: Option := Option of Integer(some: 1);
let val = if x is Option(some: val) then val else 0;

//let x := List of Integer [1, 2, 3];

