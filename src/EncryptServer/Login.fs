namespace EncryptServer
open EncryptServer.AssymetricEncryption
open System
open Newtonsoft.Json

module Login = 
    type LoginStartRequest =
        {
            KeyRingId: KeyRingId // not encrypted
        }

    module LoginStartRequest =
          let toJson (loginStartRequest: LoginStartRequest) = 1
                
            

    type LoginStartResponse =  //signed by server encrypted for client
        {
            LoginId: Id
            KeyRingId: KeyRingId //
            TimeStamp: DateTime
        }
    type LoginFinishRequest = //encrypted för server and signed by client
        {
            startResponse: LoginStartResponse 
        }
    type LoginFinishResponse =
        {
            token: string //JWT token from server with claims to access included Identity
        }

  

    

