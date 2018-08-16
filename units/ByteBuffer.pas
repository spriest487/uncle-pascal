unit ByteBuffer

interface

uses
    System.*

type ByteBuffer = class
    Bytes: ^Byte
    Count: NativeUInt
end

function EmptyByteBuffer: ByteBuffer
function ByteBufferFromBytes(bytes: ^Byte; len: NativeUInt): ByteBuffer
function ByteBufferWithLength(len: NativeUInt): ByteBuffer

function Disposable.Dispose(self: ByteBuffer)

function Concat(self: ByteBuffer; other: ByteBuffer): ByteBuffer
function Data(self: ByteBuffer): ^Byte
function Length(self: ByteBuffer): NativeUInt

implementation

function EmptyByteBuffer: ByteBuffer =
    result := (Bytes: nil; Count: 0)

function ByteBufferFromBytes(bytes: ^Byte; len: NativeUInt): ByteBuffer
begin
    result := (
        Bytes: GetMem(len)
        Count: len
    )

    for let off = NativeUInt(0) to len do
        result.Bytes[off] := bytes[off]
end

function ByteBufferWithLength(len: NativeUInt): ByteBuffer 
begin
    let var bytes := GetMem(len)
    for let i = NativeUInt(0) to len do
        bytes[i] := 0
    
    result := (Bytes: bytes; Count: len)
end

function Disposable.Dispose(self: ByteBuffer)
begin
    if self.Bytes <> nil then FreeMem(self.Bytes)
    
    self.Bytes := nil
    self.Count := 0
end

function Concat(self: ByteBuffer; other: ByteBuffer): ByteBuffer
begin
    let count = self.Count + other.Count
    let bytes = GetMem(count)

    result := (Bytes: bytes; Count: count)
end

function Data(self: ByteBuffer): ^Byte = 
    result := self.Bytes

function Length(self: ByteBuffer): NativeUInt = 
    result := self.Count

end.