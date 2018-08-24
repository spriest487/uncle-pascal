unit IO

interface

uses 
    System.*
    ByteBuffer.ByteBuffer

type FileStream = class
    FileHandle: Pointer
    Status: Int32
end

type InStream = interface
    function Read(self: Self; buf: ByteBuffer): NativeUInt
    function Ok(self: Self): Boolean
end

const 
    // todo: finish implementing enums!
    OK: NativeInt = 0
    EOF: NativeInt = 1
    BAD: NativeInt = 2

    READ: NativeInt = 0
    WRITE: NativeInt = 1

function  OpenFile(filename: String;  mode: NativeInt): FileStream

function Disposable.Dispose(self: FileStream)
function InStream.Read(self: FileStream; buf: ByteBuffer): NativeUInt
function InStream.Ok(self: FileStream): Boolean

function FOpen(filenameCStr: ^Byte; modeCStr: NativeInt): Pointer
function FClose(handle: Pointer)
function FRead(handle: Pointer; buf: ^Byte; len: NativeUInt): NativeUInt
function FEof(handle: Pointer): Boolean

implementation

function OpenFile(filename: String;  mode: NativeInt): FileStream
begin
    let fileNameBufLen = filename.Length() + NativeUInt(1)
    let fileNameCStr = GetMem(fileNameBufLen)
    filename.ToCString(fileNameCStr, fileNameBufLen)

    let handle = FOpen(fileNameCStr, mode)
    if handle = nil then
        result := (FileHandle: nil; Status: BAD)
    else
        result := (FileHandle: handle; Status: OK)

    FreeMem(fileNameCStr)
end

function InStream.Ok(self: FileStream): Boolean
begin
    result := (self.FileHandle <> nil) and self.Status = OK
end

function InStream.Read(self: FileStream; buf: ByteBuffer): NativeUInt
begin
    if self.Status <> OK then begin
        result := 0
    end
    else begin
        result := FRead(self.FileHandle, buf.Data(), buf.Length())

        if result <> buf.Length() then 
            if FEof(self.FileHandle) then
            begin
                self.Status := EOF
            end
            else begin
                self.Status := BAD
            end
    end
end

function Disposable.Dispose(self: FileStream)
begin
    if self.FileHandle <> nil then begin
        FClose(self.FileHandle)
        self.FileHandle := nil
    end
end

end.