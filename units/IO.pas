unit IO

interface

uses System.*

type FileStream = class
    FileHandle: Pointer
end

type InStream = interface
    function Read(self: Self; buf: ^Byte; count: NativeUInt): NativeUInt
    function Ok(self: Self): Boolean
end

const 
    READ: NativeInt = 0
    WRITE: NativeInt = 1

function  OpenFile(filename: String;  mode: NativeInt): FileStream

function Disposable.Dispose(self: FileStream)
function InStream.Read(self: FileStream; buf: ^Byte; count: NativeUInt): NativeUInt
function InStream.Ok(self: FileStream): Boolean

function FOpen(filenameCStr: ^Byte; modeCStr: NativeInt): Pointer
function FClose(handle: Pointer)

implementation

function OpenFile(filename: String;  mode: NativeInt): FileStream
begin
    let fileNameBufLen = filename.StringLength() + 1
    let fileNameCStr = GetMem(fileNameBufLen)
    filename.StringToCString(fileNameCStr, fileNameBufLen)

    result := (FileHandle: FOpen(fileNameCStr, mode))

    FreeMem(fileNameCStr)
end

function InStream.Ok(self: FileStream): Boolean
begin
    result := self.FileHandle <> nil
end

function InStream.Read(self: FileStream; buf: ^Byte; count: NativeUInt): NativeUInt
begin
    WriteLn('Reading')
    result := 0
end

function Disposable.Dispose(self: FileStream)
begin
    if self.FileHandle <> nil then begin
        FClose(self.FileHandle)
        self.FileHandle := nil
    end
end

end.