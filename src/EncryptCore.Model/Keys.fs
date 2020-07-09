namespace EncryptCore.Model
open EncryptCore.SymmetricEncryption
open EncryptCore.AssymetricEncryption


type PublicEncryptionKey = private | PublicEncryptionKey of PublicCsp
type PublicEncryptionKeyString = private | PublicEncryptionKeyString of string
type PublicLoginKey = private | PublicLoginKey of PublicCsp
type PublicLoginKeyString = private | PublicLoginKeyString of string
type EncryptionKeyPair = private | EncryptionKeyPair of KeyPairCsp
type EncryptionKeyPairString = private | EncryptionKeyPairString of string
type LoginKeyPair = private | LoginKeyPair of KeyPairCsp
type LoginKeyPairString = private | LoginKeyPairString of string

module EncryptionKeyPair =
    let create =
       KeyPairCsp.create >> EncryptionKeyPair
    let toEncryptionKeyPairString (EncryptionKeyPair kp) =
        KeyPairCsp.toB64String kp |> EncryptionKeyPairString

module EncryptionKeyPairString =
    let toEncryptionKeyPair (EncryptionKeyPairString s) =
        KeyPairCsp.fromB64String s |> EncryptionKeyPair

module LoginKeyPair =
    let create =
        KeyPairCsp.create >> LoginKeyPair
    let toLoginKeyPairString (LoginKeyPair kp) =
        KeyPairCsp.toB64String kp |> LoginKeyPairString

module LoginKeyPairString =
    let toLoginKeyPair (LoginKeyPairString s) =
        
module PublicLoginKey =
    let extract (LoginKeyPair key) =
        key |> PublicCsp.fromKeyPair  |> PublicLoginKey

module PublicEncryptionKey =
    let extract (EncryptionKeyPair key) =
        key |> PublicCsp.fromKeyPair  |> PublicEncryptionKey

type PrivateAccount =
 {
    identity: Identity
    encryptionKeyPair : EncryptionKeyPair
    loginKeyPair : LoginKeyPair
 }

 module private PrivateAccount =
    let create() =
        {
            identity = Identity.create()
            encryptionKeyPair = EncryptionKeyPair.create()
            loginKeyPair = LoginKeyPair.create()
        }
       
 //signerad hash
type PublicAccount =
 {
    identity: Identity
    encryptionPublicKey: PublicEncryptionKey
    loginPublicKey : PublicLoginKey
 }

 module private PublicAccount =
     let fromPrivate (account: PrivateAccount) =
        let result =
            {
                identity = account.identity
                encryptionPublicKey = PublicEncryptionKey.extract account.encryptionKeyPair
                loginPublicKey = PublicLoginKey.extract account.loginKeyPair
            }
        result
        

type ServerPrivateAccount = private | ServerPrivateAccount of PrivateAccount
type ServerPublicAccount = private | ServerPublicAccount of PublicAccount

module ServerPrivateAccount =
    let create = 
        PrivateAccount.create >> ServerPrivateAccount

module ServerPublicAccount =
    let fromPrivate (ServerPrivateAccount privateAccount) =
        PublicAccount.fromPrivate privateAccount
        |> ServerPublicAccount

type ClientPrivateAccount = private | ClientPrivateAccount of PrivateAccount
type ClientPublicAccount = private | ClientPublicAccount of PublicAccount

module ClientPrivateAccount =
    let create = 
        PrivateAccount.create >> ClientPrivateAccount

module ClientPublicAccount =
    let fromPrivate (ClientPrivateAccount privateAccount) =
        PublicAccount.fromPrivate privateAccount
        |> ClientPublicAccount

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

//module ClientAccount =
//    let create =
//        //får till tilbaka signerat konto
//        //head key kan alltid signera
//        //hämta headkey från
