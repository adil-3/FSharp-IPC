module Properties

open System.Runtime.InteropServices

type hdl = nativeint
type MProps = | Present = 19 | NButtons = 43 | VWheel = 75 | HWheel = 91

[<DllImport("user32")>] extern void MessageBox(hdl, string text, string cap, int)
[<DllImport("user32")>] extern int GetSystemMetrics(MProps)

let mPresent () = GetSystemMetrics MProps.Present > 0
let mVWheel () = GetSystemMetrics MProps.VWheel > 0
let mHWheel () = GetSystemMetrics MProps.HWheel > 0
let mButtonCount () = GetSystemMetrics MProps.NButtons