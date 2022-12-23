open System.Net
open System.Net.Sockets

open System.Threading
open Network
open Telemetry

let endpoint = IPEndPoint(IPAddress.Loopback, 10101)

do
use object = new Mutex(false, "24-TermApp-s2")
if object.WaitOne(0) |> not then
    MessageBox(0n, "Another instance is already running", "", 0x40)
    failwith "Another instance is already running"

use listener = new Socket(endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
listener.Bind endpoint
listener.Listen 100
printfn "Server #2 has started"

seq {
    for n = 100 downto 1 do
    task {
        while true do
        use! ds = listener.AcceptAsync()
        display n "CONNECTED"
        let data = Array.zeroCreate<byte> rSize
        while not (ds.Poll(10, SelectMode.SelectRead) && ds.Available = 0) do
        match! ds.ReceiveAsync data with
        | x when x = rSize ->
            let data = deserialize data
            display n $"#Requested: {data.v}"
            if data.rType = RType.Request then
                let response = match data.v with
                               | TRequest.PID -> string <| getPID ()
                               | TRequest.UMTime -> string <| getUModeTimeSec ()
                               | _ -> failwith "Internal error"
                let! _ = TData(message = response) |> serialize |> ds.SendAsync
                display n $"Sent: {response}"    
        | _ -> display n "DISCONNECTED"
    } |> Async.AwaitTask
} |> Async.Parallel |> Async.RunSynchronously |> ignore

object.ReleaseMutex()