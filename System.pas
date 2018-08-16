unit System

interface

implementation

//function StringConcat(a: ^String, b: ^String): ^String
//begin
//    let emptyA := a == 0 or a.Length == 0
//    let emptyB := b == 0 or b.Length == 0

//    if emptyA and emptyB then
//        result := ""
//    else if emptyA
//        result := StringFromBytes(B.Chars, B.Length)
//    else if emptyB
//        result := StringFromBytes(A.Chars, A.Length)
//    else begin
//        result := String.GetMem()
//            result.Length := a.Length + b.Length
//            result.Chars := GetMem(totalLen)
//
//            for c := 0 to a.Length do
//                ^(result.Chars + c) := ^(a.Chars + c)
//
//            for c := 0 to b.Length do
//                ^(result.Chars + a.Length + c) := (b.Chars + c)
//    end
//end

end.