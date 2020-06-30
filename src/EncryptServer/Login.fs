namespace EncryptServer
open EncryptServer.AssymetricEncryption
open System
open Newtonsoft.Json.Linq

module Login = 
    type LoginStartRequest =
        {
            KeyRingId: KeyRingId // not encrypted
        }


    type LoginId = LoginId of Id
       
    type LoginStartResponse =  //signed by server encrypted for client
        {
            LoginId: Id
            KeyRingId: KeyRingId //
            TimeStamp: DateTime
        }

    //module LoginStartResponse = 
    //    let private LoginId = "KeyRingId"
    //    let private keyRingId = "KeyRingId"
    //    let private TimeStamp = "KeyRingId"
    //    let toSendJson
        
    type LoginFinishRequest = //encrypted för server and signed by client
        {
            startResponse: LoginStartResponse 
        }
    type LoginFinishResponse =
        {
            token: string //JWT token from server with claims to access included Identity
        }

  

    

