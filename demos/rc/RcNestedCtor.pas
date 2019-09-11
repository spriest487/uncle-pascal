type Inner = class
    val: Integer;
end;

type Outer = class
    inner: Inner
end;

let outer := Outer(
    inner: Inner(
        val: 987
    )
)