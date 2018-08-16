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
    INIT_TIMER                  = $00000001
    INIT_AUDIO                  = $00000010
    INIT_VIDEO                  = $00000020
    INIT_JOYSTICK               = $00000200
    INIT_HAPTIC                 = $00001000
    INIT_GAMECONTROLLER         = $00002000
    INIT_EVENTS                 = $00004000
    INIT_NOPARACHUTE            = $00100000

    WINDOWPOS_UNDEFINED         = $1FFF0000

    EVENT_QUIT                  = $100
    EVENT_LASTEVENT             = $FFFF
    
    RENDERER_SOFTWARE           = $1
    RENDERER_ACCELERATED        = $2
    RENDERER_PRESENTVSYNC       = $4
    RENDERER_TARGETTEXTURE      = $8

type
    Event = record
        case Type: UInt32 of
            EVENT_QUIT: ()

            { all events are 56 bytes (as per SDL_events.h), and the first 4 bytes
            of each variant are the type tag }
            EVENT_LASTEVENT: (Padding: array[0..52] of Byte)
    end

    Window = Pointer
    Renderer = Pointer

function Init(flags: UInt32): Int32; external name 'SDL_Init'; cdecl
procedure Quit; external name 'SDL_Quit'

function CreateWindow(title: ^Byte; x, y, width, height: Int32; flags: UInt32): Window; external name 'SDL_CreateWindow'; cdecl
procedure DestroyWindow(window: Window); external name 'SDL_DestroyWindow'; cdecl

function CreateRenderer(window: Window; rendererIndex: Int32; flags: UInt32): Renderer; external name 'SDL_CreateRenderer'; cdecl
function SetRenderDrawColor(renderer: Renderer; r, g, b, a: Byte): Int32; external name 'SDL_SetRenderDrawColor'; cdecl
function RenderClear(renderer: Renderer): Int32; external name 'SDL_RenderClear'; cdecl
procedure RenderPresent(renderer: Renderer); external name 'SDL_RenderPresent'; cdecl

function PollEvent(event: ^Event): Int32; external name 'SDL_PollEvent'; cdecl
procedure Delay(ms: UInt32); external name 'SDL_Delay'; cdecl

implementation
end.
