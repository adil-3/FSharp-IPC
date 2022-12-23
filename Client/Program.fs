open Elmish
open Raylib_CsLo

open type Raylib
open type RayGui
open type ConfigFlags

open Network
open Network.Server

let title = "Inter-Process Communication"

let s1, s2 = [| "Count mouse buttons", "Buttons"
                "Vertical wheel check", "Present"; "Horizontal wheel check", "Present" |]
             |> Array.zip <| [| 1..3 |],
             [| "Server PID", "PID"; "Uptime in user mode", "UMode (sec)" |] |> Array.zip <| [| 0..1 |]

let w, h = 600, 510
[ FLAG_MSAA_4X_HINT; (*FLAG_WINDOW_UNDECORATED;*) FLAG_WINDOW_TRANSPARENT ] |> List.iter SetConfigFlags

InitWindow(w, h, "TermApp")
SetTargetFPS 30
GuiSetStyle(0, 16, 20)

let ax, pn = 50, 20
let hs, sp = 54, 36
let sw, sh = w / 2 - sp, 60
let lb, rb = pn, w - pn - sw

let BG, TC = Color(235, 235, 245, 255), Color(220, 110, 90, 230)
let CA, CF = Color(210, 180, 10, 255), Color(215, 40, 40, 255)
let G1, G2 = Color(63, 100, 220, 254), Color(176, 106, 176, 255)
let G3, G4 = Color(63, 88, 180, 254), Color(167, 67, 167, 255)
let tW, tH = MeasureText(title, 24), 24
let cW, cH = MeasureText("Server _", 20), 20

let px = ax + tH
let bh = px + sh + hs
let bf, wy = bh + 90, px + cH + 15
let w1, w2, wl = (lb, wy, G1, G2), (rb, wy, G3, G4), wy + (sh - cH) / 2 + 1

let inline button (text: string) x y cb =
    if GuiButton(Rectangle(single x - 1f, single y, single sw + 2f, 50f), text) then cb ()

type State = { fst: Server; snd: Server }

type Msg = | Unload | S1 of Server.Msg | S2 of Server.Msg | Continue
    
let init () =
    let (fst, c1), (snd, c2) = initServer 10100, initServer 10101
    { fst = fst; snd = snd }, Cmd.batch [ Cmd.map S1 c1; Cmd.map S2 c2 ]

let inline bText server =
    match server.state with
    | CV.Closed -> "Connect" | CV.Connecting -> "Connecting"
    | CV.Open -> "Disconnect" | CV.Executing -> "Disconnecting"
    | CV.Broken -> "Try Again"  | _ -> failwith ""
          
let inline window server (x, y, c, d) =
    match server.state with
    | CV.Closed -> DrawRectangle(x, y, sw, sh, LIGHTGRAY)
    | CV.Connecting -> DrawRectangle(x, y, sw, sh, CA)
    | CV.Executing -> DrawRectangle(x, y, sw, sh, VIOLET)
    | CV.Open | CV.Fetching -> DrawRectangleGradientH(x, y, sw, sh, c, d)
    | _ -> DrawRectangle(x, y, sw, sh, CF)

let bind state ev dsp gui = if state.lock then GuiDisable()
                            gui <| (fun _ -> dsp ev)
                            if state.lock then GuiEnable()
                         
let update msg state =
    match msg with
    | S1 msg' ->
        let ret, cmd = update msg' state.fst
        { state with fst = ret }, Cmd.map S1 cmd
    | S2 msg' ->
        let ret, cmd = update msg' state.snd
        { state with snd = ret }, Cmd.map S2 cmd
    | _ -> state, []

let view state dispatch =
    BeginDrawing()
    ClearBackground(BG)
    DrawText(title, (w - tW) / 2, 30, tH, TC)
    DrawText("Server 1", lb + (sw - cW) / 2, px + 5, 20, DARKGRAY)
    DrawText("Server 2", rb + (sw - cW) / 2, px + 5, 20, DARKGRAY)
    
    window state.fst w1; window state.snd w2
    button (bText state.fst) lb bh |> bind state.fst (S1 Master) dispatch 
    button (bText state.snd) rb bh |> bind state.snd (S2 Master) dispatch 

    if state.fst.state = CV.Open then
        s1 |> Array.iteri (fun i ((c, r), l) -> button c lb (bf + i * 70) (fun _ -> dispatch (Request (l, r) |> S1)))
        DrawText(state.fst.terminal, lb + (sw - MeasureText(state.fst.terminal, cH)) / 2, wl, cH, WHITE)
    if state.snd.state = CV.Open then
        s2 |> Array.iteri (fun i ((c, r), l) -> button c rb (bf + i * 70) (fun _ -> dispatch (Request (l, r) |> S2)))
        DrawText(state.snd.terminal, rb + (sw - MeasureText(state.snd.terminal, cH)) / 2, wl, cH, WHITE)

    EndDrawing()
    dispatch <| if WindowShouldClose() then Unload else Continue

Program.mkProgram init update view
|> Program.withTermination (fun t -> t = Unload)
       (fun x -> CloseWindow(); x.fst.socket.Close(); x.snd.socket.Close())
|> Program.run