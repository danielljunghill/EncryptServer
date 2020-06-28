// Learn more about F# at http://fsharp.org

open System
open EncryptServer
[<EntryPoint>]
let main argv =

    printfn "Hello World from F#!"
    let v = Test.kb
    Console.ReadLine() |> ignore
    0 // return an integer exit code

