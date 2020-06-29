namespace EncryptServer
open System

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