unit Vector

interface

uses System.*

type
  Element = Byte
  Index = NativeInt

  Vector = class
    Elements: ^Element
    Length: Index
  end

function New: Vector
function Disposable.Dispose(self: Vector)

function Add(self: Vector; p: Element)
function AddAll(self: Vector; other: Vector)

implementation

function New: Vector =
    result := (
        Elements: nil
        Length: 0
    )

function Disposable.Dispose(self: Vector) =
    FreeMem(self.Elements)

function Add(self: Vector; p: Element)
begin
  let var newElements := GetMem(self.Length + 1)

  if self.Elements <> nil then
  begin
    for let i = 0 to self.Length do
      newElements[i] := self.Elements[i]

    FreeMem(self.Elements)
  end

  self.Length := self.Length + 1
  self.Elements := newElements

  self.Elements[self.Length - 1] := p
end

function AddAll(self: Vector; other: Vector) =
  for let i = 0 to other.Length do
    self.Add(other.Elements[i])

end.