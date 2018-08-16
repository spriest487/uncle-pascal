unit Vector

uses System

interface

type Vector = record
  Elements: ^System.Byte
  Length: System.Integer
end

function Create: Vector
begin
  result.Elements := 0
  result.Length := 0
end

procedure Add(self: ^Vector; p: System.Byte)
var
  i: System.Integer
begin
  let newElements := System.GetMem(self.Length + 1)

  if self.Elements <> 0 then
  begin
    for i := 0 to self.Length do
    ^(newElements + i) := ^(self.Elements + i)


    System.FreeMem(self.Elements)
  end

  self.Length := self.Length + 1
  self.Elements := newElements

  ^(self.Elements + (self.Length - 1)) := p
end

procedure AddAll(self: ^Vector; other: ^Vector)
var
  i: System.Integer
begin
  for i := 0 to other.Length do
    Add(self, ^(other.Elements + i))
end

implementation
end.