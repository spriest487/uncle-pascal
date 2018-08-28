uses System.*

type
    Vehicle = record
      Color: String

      case Kind: Int32 of
        0: (
          Wheels: Int32
        )

        1: (
          Wings: Boolean
          Propellor: Boolean
        )
    end

function DisplayVehicle(vehicle: Vehicle)
var
  description: String
begin
  description := vehicle.Color
  if vehicle.Kind = 1 then begin
    description := description 
      + ' car with '
      + StringFromInt(vehicle.Wheels)
      + ' wheels'
  end
  else begin
    description := description + ' plane'

    if vehicle.Wings then
      description := description + ' with wings'

    if vehicle.Propellor then
      description := description + ' with a propellor'
  end

  WriteLn(description)
end

var 
  car: Vehicle
  plane: Vehicle

begin
  car.Color := 'red'
  car.Kind := 1
  car.Wheels := 4

  plane.Color := 'white'
  plane.Kind := 2
  plane.Wings := true
  plane.Propellor := false

  DisplayVehicle(car)
  DisplayVehicle(plane)
end