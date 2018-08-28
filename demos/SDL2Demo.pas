uses
    SDL
    System.*

function RunWindow(window: SDL.Window; renderer: SDL.Renderer)
begin
    let var quit := false
    let var nextEvent: SDL.Event := ()

    while not quit do
    begin
        nextEvent.PollEvent()

        if nextEvent.Type = SDL.EVENT_QUIT then
            quit := true
        else
        begin
            SDL.RenderClear(renderer)
            SDL.RenderPresent(renderer)
        end
    end
end

begin
    let title = 'Test Window'
    let titleCStr = GetMem(title.Length() + 1)
    
    if not title.ToCString(titleCStr, title.Length() + 1) then
        raise 'copying title failed'

    if SDL.Init(SDL.INIT_VIDEO) <> 0 then
        raise 'initializing SDL failed'

    let window = SDL.CreateWindow(titleCStr,
        Int32(SDL.WINDOWPOS_UNDEFINED),
        Int32(SDL.WINDOWPOS_UNDEFINED),
        800,
        600,
        0)

    if window = nil then
        raise 'creating window failed'

    let renderer = SDL.CreateRenderer(window, 0, 0)
    SDL.SetRenderDrawColor(renderer, 100, 149, 237, 255)
    
    RunWindow(window, renderer)

    SDL.DestroyWindow(window)
    SDL.Quit()

    FreeMem(titleCStr)
end