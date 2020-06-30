namespace EncryptServer
open System
open Newtonsoft.Json.Linq

type Identity = | Identity of Id
module Identity =
    let toString  =
        fun (Identity id) ->  id
        >> Id.toString
    let create =
        Id.create >> Identity
    let toByteArray =
        toString >> Convert.ToByte
    let toBase64String  =
         toString 
        >> System.Text.Encoding.UTF8.GetBytes
        >> Convert.ToBase64String
    let fromBase64String  =
       Id.fromBase64String
       >> Identity

    module Json =
        let private name = "Identity"
        let asJProperty identity =  JProperty(name, toBase64String identity)
        let fromJObject  =
            fun (kr: JObject) -> kr.[name] 
            >> string
            >> fromBase64String
          