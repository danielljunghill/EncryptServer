namespace EncryptServer
open EncryptServer.AssymetricEncryption
open System
open Newtonsoft.Json.Linq


module PublicCsp =
     module Json =
        let private name = "PublicCsp"
        let asJProperty pc =  JProperty(name, PublicCsp.toB64String pc)
        let fromJObject  =
            fun (kr: JObject) -> kr.[name] 
            >> string
            >> PublicCsp.fromB64String

module KeyPairCsp =
    module Json =
        let private name = "KeyPairCsp"
        let asJProperty kp  =  JProperty(name, KeyPairCsp.toB64String kp)
        let fromJObject  =
            fun (kr: JObject) -> kr.[name] 
            >> string
            >> KeyPairCsp.fromB64String

module KeyRingId =
    module Json =
        let private name = "KeyRingId"
        let asJProperty (KeyRingId id)  =  JProperty(name, Id.toBase64String id)
        let fromJObject  =
            fun (kr: JObject) -> kr.[name] 
            >> string
            >> Id.fromBase64String 
            >> KeyRingId


