uses System.*

type
  Dog = record
    Name: String
    Age: Int32
  end

function CreateDog(name: String; age: Int32): Dog =
  result := (Age: age; Name: name)

begin
  let spot = CreateDog('Spot', 2)
  let dot = CreateDog('Dot', 3)

  with spot do
  begin
    WriteLn('Name: ' + Name + ', Age: ' + StringFromInt(Age))
  end

  with dot do
  begin
    WriteLn('Name: ' + Name + ', Age: ' + StringFromInt(Age))
  end
end