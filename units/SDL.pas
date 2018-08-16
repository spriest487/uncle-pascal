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

    WINDOWPOS_UNDEFINED =       $1FFF0000

    EVENT_QUIT = $100
    EVENT_LASTEVENT = $FFFF

type
    EventType = (
        EVENT_QUIT,
        EVENT_LASTEVENT
    )

    Event = record
        case Type: EventType of
            EVENT_QUIT: ()
            EVENT_LASTEVENT: (Padding: array[0..56] of Byte)
    end

function Init(flags: UInt32): Int32
    ; external name 'SDL_Init'
    ; cdecl

procedure Quit
    ; external name 'SDL_Quit'

function CreateWindow(title: ^Byte;
    x, y, width, height: Int32;
    flags: UInt32): Pointer
    ; external name 'SDL_CreateWindow'
    ; cdecl

procedure DestroyWindow(window: Pointer)
    ; external name 'SDL_DestroyWindow'
    ; cdecl

function PollEvent(event: Pointer): Int32
    ; external name 'SDL_PollEvent'
    ; cdecl

procedure Delay(ms: UInt32); 
    external name 'SDL_Delay'
    ; cdecl

implementation
end.
