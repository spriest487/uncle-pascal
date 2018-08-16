program ReadFile

uses IO

const FILENAME = 'demos/ReadFile.pas'

begin
        let file = IO.OpenFile(FILENAME, IO.READ)
        if not  IO.InStream.Ok(file) then 
                raise 'opening file ' + FILENAME + ' failed!'
end.