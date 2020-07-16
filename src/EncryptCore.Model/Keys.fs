namespace EncryptCore.Model
open EncryptCore.SymmetricEncryption
open EncryptCore.AssymetricEncryption
open EncryptCore
open EncryptCore.Hash

type KeyId = | KeyId of Identity

module KeyId =
    let create =
        Identity.create
        >> KeyId
    let toByteArray (KeyId (Identity bts)) = bts
    //let toBu (KeyId (Identity bts)) = bts
    //let create keyPair =
    //    Identity.create()
    //    |> KeyId
    //    |> fun identity -> SignedValue.create map identity keyPair

type PublicKeyId = private | PublicKeyId of KeyId
module PublicKeyId = 
    let toByteArray (PublicKeyId (KeyId (Identity bts)))= bts
        
    let create =
        KeyId.create
        >> PublicKeyId

    let toSigned  =
        Signed.create toByteArray 

    let createSigned  =
        create()
        |> (fun value -> toSigned value)

    let validate  =
        Signed.validate toByteArray


type PrivateKey =
    {
        id: Signed<PublicKeyId>
        keyPairCsp: KeyPairCsp
    }
        
type PublicKey = 
    {
        id: Signed<PublicKeyId>
        publicCsp: PublicCsp
    }

module PublicKey =
    let validate (publicKey: PublicKey) =
        PublicKeyId.validate publicKey.id publicKey.publicCsp 


module PrivateKey =
    let create() =
        let kp = KeyPairCsp.create()
        let signedId = PublicKeyId.createSigned kp
        {
            id = signedId
            keyPairCsp = kp
        }
    let toPublicKey (privateKey:PrivateKey) =
        {
            id = privateKey.id
            publicCsp = KeyPairCsp.toPublicCsp privateKey.keyPairCsp
        }

    let validate (privateKey: PrivateKey) =
        PublicKeyId.validate privateKey.id (KeyPairCsp.toPublicCsp privateKey.keyPairCsp) 


type PublicEncryptionKey = private | PublicEncryptionKey of PublicKey
type PublicLoginKey = private | PublicLoginKey of PublicKey
type PublicAccountKey = private | PublicAccountKey of PublicKey
type EncryptionKeyPair = private | EncryptionKeyPair of PrivateKey
type LoginKeyPair = private | LoginKeyPair of PrivateKey  
type PrivateAccountKey = private | PrivateAccountKey of PrivateKey

type PrivateAccount =
 {
    encryptionKeyPair : Signed<EncryptionKeyPair>
    loginKeyPair : LoginKeyPair
    accountKey: PrivateAccountKey
 }
 member x.AccountId = 
    let (PrivateAccountKey key) = x.accountKey
    let (PublicKeyId (KeyId identity)) = key.id.value
    identity

 type SignedPrivateAccount = private | SignedPrivateAccount of Signed<PrivateAccount> 

// module PrivateAccount =
//    let toByteArray
//    let sign


// module private PrivateAccount =
//    let create() =
//        {
//            identity = Identity.create()
//            encryptionKeyPair = EncryptionKeyPair.create()
//            loginKeyPair = LoginKeyPair.create()
//        }
       
// //signerad hash
//type PublicAccount =
// {
//    identity: Identity
//    encryptionPublicKey: PublicEncryptionKey
//    loginPublicKey : PublicLoginKey
// }

// module private PublicAccount =
//     let fromPrivate (account: PrivateAccount) =
//        let result =
//            {
//                identity = account.identity
//                encryptionPublicKey = PublicEncryptionKey.extract account.encryptionKeyPair
//                loginPublicKey = PublicLoginKey.extract account.loginKeyPair
//            }
//        result
        

//type ServerPrivateAccount = private | ServerPrivateAccount of PrivateAccount
//type ServerPublicAccount = private | ServerPublicAccount of PublicAccount

//module ServerPrivateAccount =
//    let create = 
//        PrivateAccount.create >> ServerPrivateAccount

//module ServerPublicAccount =
//    let fromPrivate (ServerPrivateAccount privateAccount) =
//        PublicAccount.fromPrivate privateAccount
//        |> ServerPublicAccount

//type ClientPrivateAccount = private | ClientPrivateAccount of PrivateAccount
//type ClientPublicAccount = private | ClientPublicAccount of PublicAccount

//module ClientPrivateAccount =
//    let create = 
//        PrivateAccount.create >> ClientPrivateAccount

//module ClientPublicAccount =
//    let fromPrivate (ClientPrivateAccount privateAccount) =
//        PublicAccount.fromPrivate privateAccount
//        |> ClientPublicAccount

//type ServerAcccount = 
//    {
//        server: ServerPrivateAccount
//        client : ClientPublicAccount
//    }

//type ClientAccount =
//    {
//        server: ServerPublicAccount
//        client : ClientPrivateAccount
//    }

////module ClientAccount =
//    let create =
//        //får till tilbaka signerat konto
//        //head key kan alltid signera
//        //hämta headkey från
