unit SDL

{$linklib SDL2}
{$ifdef MSWINDOWS}
    {$linklib user32}
    {$linklib shell32}
    {$linklib advapi32}
    {$linklib gdi32}
    {$linklib winmm}
    {$linklib imm32}
    {$linklib ole32}
    {$linklib oleaut32}
    {$linklib version}
    {$linklib uuid}
{$endif}

interface

uses System.*

const 
    INIT_TIMER =                $00000001
    INIT_AUDIO =                $00000010
    INIT_VIDEO =                $00000020
    INIT_JOYSTICK =             $00000200
    INIT_HAPTIC =               $00001000
    INIT_GAMECONTROLLER =       $00002000
    INIT_EVENTS =               $00004000
    INIT_NOPARACHUTE =          $00100000
    WINDOWPOS_UNDEFINED =       536805376 // $1FFF0000

function Init(flags: UInt32): Int32
procedure Quit

function CreateWindow(
    title: ^Byte;
    x, y, width, height: Int32;
    flags: UInt32
): Pointer

procedure DestroyWindow(window: Pointer)

function PollEvent(event: Pointer): Int32

procedure Delay(ms: UInt32)

implementation
end.
