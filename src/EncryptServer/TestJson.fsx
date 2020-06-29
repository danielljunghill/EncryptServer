open System
#r @"C:\Users\dalj\.nuget\packages\newtonsoft.json\12.0.3\lib\net45\Newtonsoft.Json.dll"
#load "Library.fs"
open Newtonsoft.Json

type KeyRing =
    {
        KeyRingId: string
    }
let v = EncryptServer.Test.kb