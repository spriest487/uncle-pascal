program SDL2Demo

uses
    SDL
    System.*

begin
    let title = 'Test Window'
    let titleCStr = GetMem(title.Length + 1)
    
    if not StringToCString(title, titleCStr, title.Length + 1) then
        raise 'copying title failed'

    if SDL.Init($0000002) <> 0 then
        raise 'initializing SDL failed'

    let window = SDL.CreateWindow(titleCStr, 20, 20, 800, 600, $0)
    if window = nil then
        raise 'creating window failed'
    
    for let i = 1 to 10 do
         SDL.PollEvent(nil)

    SDL.Delay($7d0)

    SDL.DestroyWindow(window)
    SDL.Quit()

    FreeMem(titleCStr)
end.