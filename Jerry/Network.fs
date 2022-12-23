module Network

open System
open System.Runtime.InteropServices

type RType = | Info = 0 | Request = 1 | Response = 2
type MRequest = | Present = 0 | BCount = 1 | VWheel = 2 | HWheel = 3

type TData = struct
    val mutable v: MRequest
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
    
let k = obj ()
let display n message = lock k (fun _ -> printfn $"worker-{n}: {DateTime.Now} {message}")