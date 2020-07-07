namespace EncryptCore.Model
open EncryptCore.SymmetricEncryption
open EncryptCore.AssymetricEncryption



type PublicEncryptionKey = private | PublicEncryptionKey of PublicCsp
type PublicLoginKey = private | PublicLoginKey of PublicCsp

type EncryptionKeyPair = private | EncryptionKeyPair of KeyPairCsp
type LoginKeyPair = private | LoginKeyPair of KeyPairCsp

module EncryptionKeyPair =
    let create =
       KeyPairCsp.create >> EncryptionKeyPair

module LoginKeyPair =
    let create =
        KeyPairCsp.create >> LoginKeyPair
  
module PublicLoginKey =
    let extrakt (EncryptionKeyPair key) =
        key |> PublicCsp.fromKeyPair  |> PublicLoginKey

module PublicEncryptionKey =
    let extrakt (LoginKeyPair key) =
        key |> PublicCsp.fromKeyPair  |> PublicEncryptionKey

type PrivateEncryptionAccount =
 {
    identity: Identity
    encryptionKeyPair : EncryptionKeyPair
    loginKeyPair : LoginKeyPair
 }
 module PrivateEncryptionAccount =
    let create =
        {
            identity = Identity.create()
            encryptionKeyPair = EncryptionKeyPair.create()
            loginKeyPair = LoginKeyPair.create()
        }
        
 //signerad hash
type PublicEncryptionAccount =
 {
    identity: Identity
    encryptionPublicKey: PublicEncryptionKey
    loginPublicKey : PublicLoginKey
 }

type ServerPrivateAccount = | ServerPrivateAccount of PrivateEncryptionAccount
type ServerPublicAccount = | ServerPublicAccount of PublicEncryptionAccount
    
type ClientPrivateAccount = | ClientPrivateAccount of PrivateEncryptionAccount
type ClientPublicAccount = | ClientPublicAccount of PublicEncryptionAccount


type ServerAcccount = 
    {
        server: ServerPrivateAccount
        client : ClientPublicAccount
    }

type ClientAccount =
    {
        server: ServerPublicAccount
        client : ClientPrivateAccount
    }

module EncryptClient =
    let create =
    //client encrypt decrypt keys
    //client login keys

