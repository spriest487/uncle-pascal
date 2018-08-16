unit Vector

uses System.*

interface

type
  Element = Byte
  Index = Integer

  Vector = class
    Elements: ^Element
    Length: Index
  end

constructor Create: Vector
begin
  result.Elements := nil
  result.Length := 0
end

destructor Destroy(self: Vector)
begin
    FreeMem(self.Elements)
end

procedure Add(self: Vector; p: Element)
begin
  let newElements := GetMem(self.Length + 1)

  if self.Elements <> nil then
  begin
    for let i := 0 to self.Length do
      ^(newElements + i) := ^(self.Elements + i)

    FreeMem(self.Elements)
  end

  self.Length := self.Length + 1
  self.Elements := newElements

  ^(self.Elements + (self.Length - 1)) := p
end

procedure AddAll(self: Vector; other: Vector)
begin
  for let i := 0 to other.Length do
    self.Add(^(other.Elements + i))
end

implementation
end.