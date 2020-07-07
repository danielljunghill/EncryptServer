namespace EncryptCore.Model
open EncryptCore.SymmetricEncryption
open EncryptCore.AssymetricEncryption



type ClientPublicKey = private | ClientPublicKey of PublicCsp

type ClientEncryptionKey = private | ClientEncryptionKey of KeyPairCsp
type ClientLoginKey = private | ClientEncryptionKey of KeyPairCsp    
type EncryptClient =
 {
    identity: Client
 }
module EncryptClient =
    //client encrypt decrypt keys
    //client login keys

