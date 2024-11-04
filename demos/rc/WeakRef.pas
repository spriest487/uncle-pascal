implementation
uses System;

type
    Child = class; 

    Parent = class of Disposable
        child: Option[Child];
        name: String;
        
        function Dispose;
    end;

    Child = class of Disposable
        parent: Option[weak Parent];

        function Dispose;
    end;
    
function Parent.Dispose;
begin
    WriteLn('disposed parent ' + self.name);
end;

function Child.Dispose;
begin
    WriteLn('disposed child');
end;

initialization
    // this will expire immediately
    var weakParent: weak Parent := Parent(name: 'Parent1'; child: Option.None());
    if weakParent is not Parent then
        WriteLn('Parent1 has expired');
    
    begin
        var child := Child(parent: Option.None());
        var parent := Parent(name: 'Parent2'; child: Option.Some(child));
        child.parent := Option.Some(parent);
        
        // this is still valid until this scope ends
        var child2: weak Child := child;
        if child2 is Child aliveChild then
            WriteLn('child is still alive');
    end;
end.
