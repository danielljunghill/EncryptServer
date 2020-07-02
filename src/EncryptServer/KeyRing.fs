namespace EncryptServer
open EncryptServer.AssymetricEncryption
open System


type KeyRingId = KeyRingId of Id

type KeyRing =
    {
        KeyRingId: KeyRingId
        KeyPairCsp: KeyPairCsp
        PublicCsp: PublicCsp
    }

//module KeyRing =
//    module Json =
//        let asJObject encryptor (kr: KeyRing) = 
//            let keyRingId = KeyRingId.Json.asJProperty kr.KeyRingId
//            let KeyPairCspEncrypt (KeyPairCsp bts) encryptor = 
//                let (encryptedValue: byte[]) = encryptor bts
       
       

type ServerKeyRing = ServerKeyRing of KeyRing
type ClientKeyRing = ClientKeyRing of KeyRing



//Anrop lassar upp sin publika nyckel 

