uses System.*

type
    Wrapper<T> = record
        Value: T
    end

begin
    let wrappedInt: Wrapper<Int32> = (Value: 123)
    let wrappedStr: Wrapper<String> = (Value: 'hello world!')

    WriteLn('wrapped int: ' + StringFromInt(wrappedInt.Value))
    WriteLn('wrapped string: ' + wrappedStr.Value)
end