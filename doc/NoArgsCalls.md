In the right context, Pascal function calls don't require an argument list.
For example the function declared as:

```
function X();
begin
end;
```

Can be called by simply writing:

```
X;
```

# Implementation

When typechecking an expression, single idents that reference functions can easily
be identified. However, typechecking an expression happens in many contexts,
including as part of a function call with a subsequent argument list. If we write
`X()`, we have to check the type of `X` first before applying the (empty, in this case)
argument to it.

This means that we can't immediately transform single idents into the normal function
calls. We wrap them instead in a special `FunctionCallNoArgs` item. This allows the
typechecker to treat these nodes differently when typechecking a call - for example,
when typechecking `X()`, `X` will first be resolved as a no-args call to `X`. The meaning
of `X` would therefore be "call the result of calling `X`", but since `X` is identifiable
as a no-args call, we instead merge it into the call-with-args node it appears in. This
makes argument lists option, as intended in Pascal, but introduces some ambiguities:

```
function X(): function: Integer;
begin
    // return some value here with a function type
end;

// case 1
X();

// case 2
var x1 := X;
var x2: function: Integer := X;
var x3: function: function: Integer := X;
```

In case 1, we call `X`, but it would also be reasonable to interpret it as a call to the _result_
of `X` - equivalent to `X()()`. The no-args unwrapping rule means that it'll always be processed
as a call to `X`, returning `function: Integer` and never a reference to X 
(`function: function: Integer`).

In case 2, the value of `X` varies depending on the context. The expression on its own will
always evaluate to a call to the function `X`. A type hint of `function: Integer` has the same
result. However, in contexts where a reference to the `X` function itself is needed, a type hint
of the matching function type will cause an ident to evaluate to a function reference to the
function instead of a call to it.