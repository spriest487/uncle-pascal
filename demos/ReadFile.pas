program ReadFile

uses 
    System.*
    IO

const 
    FILENAME = 'demos/ReadFile.pas'
    BUF_SIZE = 1024

function ReadToEnd(input: IO.InStream): String
begin
    result := ''

    let readBuf = GetMem(BUF_SIZE)

    { todo: this can't be declared inside the loop for some reason }
    let var readCount: NativeUInt := 0

    while IO.InStream.Ok(input) do begin
        readCount := input.Read(readBuf, NativeUInt(BUF_SIZE))
        WriteLn('read ' + StringFromInt(Int32(readCount)) + ' bytes')

        result := result + StringFromBytes(readBuf, NativeInt(readCount)) 
    end

    FreeMem(readBuf)
end

begin
    let file = IO.OpenFile(FILENAME, IO.READ)
    if not IO.InStream.Ok(file) then 
        raise 'opening file ' + FILENAME + ' failed!'

    let text = ReadToEnd(file)
    WriteLn(text)
end.