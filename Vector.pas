unit Vector

uses System

interface

type Vector = record
  Elements: ^System.Byte
  Length: System.Integer
end

function Create: Vector
begin
  result.Elements := nil
  result.Length := 0
end

procedure Add(self: ^Vector; p: System.Byte)
begin
  let newElements := System.GetMem(self.Length + 1)

  if self.Elements <> nil then
  begin
    for let i := 0 to self.Length do
      ^(newElements + i) := ^(self.Elements + i)

    System.FreeMem(self.Elements)
  end

  self.Length := self.Length + 1
  self.Elements := newElements

  ^(self.Elements + (self.Length - 1)) := p
end

procedure AddAll(self: ^Vector; other: ^Vector)
begin
  for let i := 0 to other.Length do
    self.Add(^(other.Elements + i))
end

implementation
end.