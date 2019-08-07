// error: class constructors are private to the declaring module
{ let c := Unit2.C(value: 123); }

// ok: record constructors are public
let r := Unit2.R(value: 123);

// ok: variant constructors are public
let vL := Unit2.V.Left();
let vR := Unit2.V.Right();

// error: record is not exported
let r2 := Unit2.R2(value: 456);