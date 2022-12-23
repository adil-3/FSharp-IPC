module Telemetry

open System.Runtime.InteropServices

type dw = uint
type hdl = nativeint
type FTime = struct val low: dw; val high: dw end

[<DllImport("user32")>] extern void MessageBox(hdl, string text, string cap, int)

[<DllImport("kernel32")>] extern hdl GetCurrentProcess()
[<DllImport("kernel32")>] extern int GetCurrentProcessId()
[<DllImport("kernel32")>]
extern bool GetProcessTimes(hdl proc, FTime&, FTime&, FTime&, FTime&)

let getPID () = GetCurrentProcessId()

let getUModeTimeSec () =
    let mutable n = FTime()
    let mutable uTime = FTime()
    GetProcessTimes(GetCurrentProcess(), &n, &n, &n, &uTime) |> ignore
    double ((uint64 uTime.high <<< 32) + uint64 uTime.low) / 10_000_000.