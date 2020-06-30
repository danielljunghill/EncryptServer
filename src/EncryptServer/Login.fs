namespace EncryptServer
open EncryptServer.AssymetricEncryption
open System
open Newtonsoft.Json.Linq

module Login = 
    type LoginStartRequest =
        {
            KeyRingId: KeyRingId // not encrypted
        }

    module KeyRing =
        

    module LoginStartRequest =
          let private keyRingId = "KeyRingId"
          let toJson (loginStartRequest: LoginStartRequest) = 
                let (KeyRingId id) = loginStartRequest.KeyRingId
                new JObject(
                    JProperty("KeyRingId", Id.toBase64String id)
                ) 
                
          let fromJson json =
                let kr = JObject.Parse(json)
                let keyRingId = 
                    kr.[keyRingId] |> string
                    |> Id.fromBase64String 
                    |> KeyRingId
                { KeyRingId = keyRingId }

    type LoginId = LoginId of Id
       
    type LoginStartResponse =  //signed by server encrypted for client
        {
            LoginId: Id
            KeyRingId: KeyRingId //
            TimeStamp: DateTime
        }

    module LoginStartResponse = 
        let private LoginId = "KeyRingId"
        let private keyRingId = "KeyRingId"
        let private TimeStamp = "KeyRingId"
        let toSendJson
        
    type LoginFinishRequest = //encrypted för server and signed by client
        {
            startResponse: LoginStartResponse 
        }
    type LoginFinishResponse =
        {
            token: string //JWT token from server with claims to access included Identity
        }

  

    

