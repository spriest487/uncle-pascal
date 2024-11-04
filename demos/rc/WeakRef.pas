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
    var weakParent: weak Parent := Parent(name: 'Parent1'; child: Option.None());
    begin
        var child := Child(parent: Option.None());
        var parent := Parent(name: 'Parent2'; child: Option.Some(child));
        child.parent := Option.Some(parent);
    end;
end.
