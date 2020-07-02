namespace EncryptServer
open EncryptServer.AssymetricEncryption
open System
open Newtonsoft.Json.Linq

module Login = 
    type LoginStartRequest =
        {
            KeyRingId: KeyRingId // not encrypted
        }
    
    type LoginStartEnvelop =
        {
            KeyRingId: string //base64 representation av KeyRingId (KeyIdentity)
        }

    type LoginId = LoginId of Id
       
    type LoginStartResponse =  //signed by server encrypted for client
        {
            LoginId: Id
            KeyRingId: KeyRingId //
            TimeStamp: DateTime
        }
    type LoginStartResponseEnvelope =
        {
            LoginStartResponse : string  //enncrypted for client string of LoginStartResponse
            signature: string //base64string of sign of startresponse non encrypted

        }

        
    type LoginFinishRequest = //encrypted för server and signed by client
        {
            startResponse: LoginStartResponse 
        }

    type LoginFinishRequestEnvelope = 
        {
            startResponseEnvelope: string //base64 of startResponse envelope
            signature: string //base64 signature for startResponse
        }
    type LoginFinishResponse =
        {
            token: string //JWT token from server with claims to access included Identity
        }

  

    

