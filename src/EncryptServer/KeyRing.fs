namespace EncryptServer
open EncryptServer.AssymetricEncryption
open System
open Newtonsoft

type KeyRingId = KeyRingId of Id

type KeyRing =
    {
        KeyRingId: KeyRingId
        KeyPairCsp: KeyPairCsp
        PublicCsp: PublicCsp
    }

type ServerKeyRing = ServerKeyRing of KeyRing
type ClientKeyRing = ClientKeyRing of KeyRing







//Anrop lassar upp sin publika nyckel 

