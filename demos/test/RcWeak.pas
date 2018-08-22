program RcWeakTest

uses System.*

type
    Person = interface
        function Name(self: Self): String
    end

    Parent = class
        Child: Person
    end

    Child = class
        Parent: weak Parent
    end

function Person.Name(self: Parent): String
begin
    result := 'Jane'
end

function Person.Name(self: Child): String
begin
    if self.Parent <> nil then
    begin
        let parentName =  self.Parent.Name()
        result := 'child of ' + parentName
    end
    else
        result := 'no parent'
end

begin
    let var child: Child := (Parent: nil)
    WriteLn('child: ' + child.Name())

    begin
        let parent: Parent = (Child: child)
        WriteLn('parent: ' + parent.Name())

        child.Parent := parent
        WriteLn('child: ' + child.Name())
        if child.Parent = nil then raise 'parent should not be disposed'
    end

    WriteLn('child: ' + child.Name())

    if child.Parent = nil then WriteLn('Ok!') else raise 'parent should be disposed'
end.