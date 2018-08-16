program ReadFile

uses 
    System.*
    ByteBuffer
    IO

const 
    FILENAME = 'demos/ReadFile.pas'
    BUF_SIZE = 1024

function ReadToEnd(input: IO.InStream): String
begin
    result := ''
    let readBuf = ByteBufferWithLength(1024)

    while input.Ok() do begin
        let readCount = input.Read(readBuf)
        result := result + StringFromBytes(readBuf.Data(), readBuf.Length()) 
    end
end

begin
    let file = IO.OpenFile(FILENAME, IO.READ)
    if not file.Ok() then 
        raise 'opening file ' + FILENAME + ' failed!'

    let text = ReadToEnd(file)
    WriteLn(text)
end.