let nothingPtr: ^Integer := nil;

var x := 1;

x := 2;
x := x + 1;
WriteLn(IntToStr(x));

var y: Integer := 123321;
var yPtr := @y;
WriteLn(IntToStr(yPtr^));

// illegal: @y returns a temporary ^Integer
//let yPtrPtr: ^^Integer := @@y;
//WriteLn(IntToStr(yPtrPtr^^));

let yPtrPtr := @yPtr;
WriteLn(IntToStr(yPtrPtr^^));

y := 9876;
WriteLn(IntToStr(yPtr^));

yPtrPtr^^ := 56654;
WriteLn(IntToStr(y));

let bothTrue := true and true;
WriteLn(if bothTrue then 'Both true' else 'Not both true');