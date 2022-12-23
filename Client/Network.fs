module Network

open System.Net
open System.Net.Sockets
open System.Runtime.InteropServices
open Elmish

type CV = Closed = 0 | Open = 1 | Connecting = 2 | Fetching = 3 | Executing = 4 | Broken = 5
type RType = | Info = 0 | Request = 1 | Response = 2

type TData = struct
    val mutable v: int
    [<MarshalAs(UnmanagedType.ByValTStr, SizeConst = 100)>]
    val mutable message: string
    val mutable rType: RType
end

let rSize = Marshal.SizeOf<TData>()

let serialize s =
    let ptr, raw = Marshal.AllocHGlobal rSize, Array.zeroCreate<byte> rSize
    Marshal.StructureToPtr(s, ptr, true)
    Marshal.Copy(ptr, raw, 0, rSize)
    Marshal.FreeHGlobal ptr
    raw
    
let deserialize (b: byte[]) =
    let ptr = Marshal.AllocHGlobal rSize
    Marshal.Copy(b, 0, ptr, rSize)
    let data: TData = Marshal.PtrToStructure ptr
    Marshal.FreeHGlobal ptr
    data
    
module Server =
    type Server = { terminal: string; socket: Socket; endpoint: IPEndPoint; state: CV; lock: bool }

    type Msg = Master | Connect | Disconnect | Connected | Disconnected | Failed
                      | Request of int * string | Completed of string
    
    let fuse state dsp = dsp <| match state.state with | CV.Closed | CV.Broken -> Connect | _ -> Disconnect
    
    let connect server = task { do! server.socket.ConnectAsync server.endpoint } 
    let disconnect server = task { server.socket.Shutdown SocketShutdown.Both; do! server.socket.DisconnectAsync true } 
    
    let request from remote server  = task {
        let data = Array.zeroCreate<byte> rSize
        let! _ = TData(rType = RType.Request, v = from) |> serialize |> server.socket.SendAsync
        match! server.socket.ReceiveAsync data with | t when t = rSize -> () | _ -> failwith "Malformed data"
        return $"{remote}: {(deserialize data).message}"
    }
    
    let initServer port =
        let endpoint = IPEndPoint(IPAddress.Loopback, port)
        let socket = new Socket(endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
        { terminal = ""; socket = socket; endpoint = endpoint; state = CV.Closed; lock = false }, []

    let error _ = Failed
    let fetch some state task = Cmd.OfTask.either task state some error
    let complete some state task = Cmd.OfTask.either task state (fun _ -> some) error
    
    let update msg state =
        match msg with
        | Master -> state, [ fuse state ]
        | Connect -> { state with state = CV.Connecting; lock = true }, complete Connected state connect
        | Connected -> { state with state = CV.Open; terminal = "*Connected*"; lock = false }, []
        | Disconnect -> { state with state = CV.Executing; lock = true }, complete Disconnected state disconnect
        | Disconnected -> fst (initServer state.endpoint.Port), []
        | Request (ch, cap) -> { state with terminal = "_" }, fetch Completed state (request ch cap)
        | Completed result -> { state with state = CV.Open; terminal = result; lock = false }, []
        | Failed -> { state with state = CV.Broken; lock = false }, []