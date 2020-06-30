namespace EncryptServer
open EncryptServer.AssymetricEncryption
open System
open Newtonsoft.Json.Linq

type KeyRingId = KeyRingId of Id

module KeyRingId =
    module Json =
        let private name = "KeyRingId"
        let asJProperty (KeyRingId id)  =  JProperty(name, Id.toBase64String id)
        let fromJObject  =
            fun (kr: JObject) -> kr.[name] 
            >> string
            >> Id.fromBase64String 
            >> KeyRingId

type KeyRing =
    {
        KeyRingId: KeyRingId
        KeyPairCsp: KeyPairCsp
        PublicCsp: PublicCsp
    }

module KeyRing =
    module Json =
        let asJObject encryptor (kr: KeyRing) = 
            let keyRingId = KeyRingId.Json.asJProperty kr.KeyRingId
            let KeyPairCspEncrypt (KeyPairCsp bts) encryptor = 
                let (encryptedValue: byte[]) = encryptor bts

        
       

type ServerKeyRing = ServerKeyRing of KeyRing
type ClientKeyRing = ClientKeyRing of KeyRing







//Anrop lassar upp sin publika nyckel 

