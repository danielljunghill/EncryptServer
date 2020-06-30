open System
#r @"C:\Users\dalj\.nuget\packages\newtonsoft.json\12.0.3\lib\net45\Newtonsoft.Json.dll"

open Newtonsoft.Json.Linq

type KeyRing =
    {
        KeyRingId: Guid
    }

module JsonLinq =
 
    let keyRingToJson =
        fun (keyRing: KeyRing) ->
            new JObject(
                new JProperty("KeyRingId",keyRing.KeyRingId)
            )
    let kr = { KeyRingId = Guid.NewGuid() }
    let krObj = keyRingToJson kr
    let fromJson  =
         JObject.Parse
        
        //let v = k.["KeyRingId"]
        //v


let test = JsonLinq.krObj.ToString()
let p = JsonLinq.fromJson(test) 
let v = p.["KeyRingId"].[""]

    


