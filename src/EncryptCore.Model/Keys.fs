namespace EncryptCore.Model
open EncryptCore.SymmetricEncryption
open EncryptCore.AssymetricEncryption
open EncryptCore
open EncryptCore.Hash
open Model.Signed
type KeyId = | KeyId of Identity

module KeyId =
    let create =
        Identity.create 128
        >> KeyId 
    let toByteArray (KeyId (Identity bts)) = bts


type PrivateKey =
    {
        id: KeyId
        keyPairCsp: KeyPairCsp
    }

module PrivateKey =
    let toByteArray (privateKey : PrivateKey) =
        Array.append (KeyId.toByteArray privateKey.id) (KeyPairCsp.toByteArray privateKey.keyPairCsp)   
    let toKeyPairsCsp (privateKey: PrivateKey) = privateKey.keyPairCsp
    let create() =
        { id = KeyId.create(); keyPairCsp = KeyPairCsp.create() }
    let toPublicCsp (privateKey: PrivateKey) = KeyPairCsp.toPublicCsp privateKey.keyPairCsp

type PublicKey =
    {
        id: KeyId
        publicCsp: PublicCsp
    }

module PublicKey =
    let toByteArray (publicKey : PublicKey) =
        Array.append (KeyId.toByteArray publicKey.id) (PublicCsp.toByteArray publicKey.publicCsp)
    let create (privateKey: PrivateKey) =
        { id = privateKey.id; publicCsp = KeyPairCsp.toPublicCsp privateKey.keyPairCsp }

type SignedPrivateKey = private | SignedPrivateKey of Signed<PrivateKey>
type SignedPublicKey = private | SignedPublicKey of Signed<PublicKey>


module SignedPrivateKey =
    //let create() =
    //    let privateKey = PrivateKey.create()
    //    let signed = Signed.createAndSign PrivateKey.toByteArray privateKey privateKey.keyPairCsp
    //    SignedPrivateKey signed
    let create (privateKey: PrivateKey)  =
        let signKey = SignKey.fromKeyPair privateKey.keyPairCsp
        let signed = Signed.createAndSign PrivateKey.toByteArray privateKey signKey
        SignedPrivateKey signed
        Signed.sign (SelfValidationKey ValidateiprivateKey.keyPairCsp)  
        >> SignedPrivateKey
    let toPrivateKey (SignedPrivateKey signedKey) = signedKey.value
    let validate (SignedPrivateKey signedKey) (signedPublicKeys: SignedPublicKey list) =
        let sigendPublicKeyToValidationKey (SignedPublicKey signedPublicKey) = Signed.ValidationKey.fromKeyPair signedPublicKey.value.publicCsp
        let selfValidationKey =  KeyPairCsp.toPublicCsp signedKey.value.keyPairCsp

        Signed.validate PrivateKey.toByteArray signedKey ( :: (signedPublicKeys |> List.map (fun signedPublicKeys -> getSignKey signedPublicKeys)))

module SignedPublicKey =
    let create (SignedPrivateKey signedPrivateKey) =
        //check if private key is valid
        let publicKey = PublicKey.create signedPrivateKey.value
        let signed = Signed.createAndSign PublicKey.toByteArray publicKey signedPrivateKey.value.keyPairCsp
        SignedPublicKey signed
    let sign (SignedPublicKey signedKey)  =
        Signed.sign signedKey  
        >> SignedPublicKey
    let toSignedKey (SignedPublicKey signedKey) = signedKey
    let toPublicKey = toSignedKey >> fun v -> v.value
    let toPublicCsp = toSignedKey >> fun v -> v.value.publicCsp
    let id = toSignedKey >> fun v -> v.value.id
    let validate signedPublicKey (signedPublicKeys: SignedPublicKey list) =
        Signed.validate PublicKey.toByteArray (toSignedKey signedPublicKey) ((toPublicCsp signedPublicKey)  :: (signedPublicKeys |> List.map (fun signedPublicKey -> toPublicCsp signedPublicKey)))




type PublicAccountKey = private | PublicAccountKey of SignedPublicKey
type PrivateAccountKey = private | PrivateAccountKey of SignedPrivateKey

module PrivateAccountKey =

    let create =
        SignedPrivateKey.create
        >> PrivateAccountKey
    let validate (PrivateAccountKey signedPrivateKey) =
        SignedPrivateKey.validate signedPrivateKey []
    let toSignedPrivateKey (PrivateAccountKey privateKey) = privateKey
    let toSignKey (PrivateAccountKey privateKey) = SignedPrivateKey.toPrivateKey privateKey
    let sign<'T> (signed: Signed<'T>) = 
        toSignedPrivateKey
        >> SignedPrivateKey.sign

module PublicAccountKey =

    let create =
        PrivateAccountKey.toSignedPrivateKey
        >> SignedPublicKey.create
        >> PublicAccountKey
    let toSignedPublicKey (PublicAccountKey publicAccountKey) = publicAccountKey
    let validate (PublicAccountKey publicAccountKey)  =
        SignedPublicKey.validate publicAccountKey []

type PrivateEncryptionKey = private | PrivateEncryptionKey of SignedPrivateKey

module PrivateEncryptionKey =

    let create (privateAccountKey: PrivateAccountKey) =
        let signedPrivateKey = SignedPrivateKey.create()
        let signKey = PrivateAccountKey.toSignKey privateAccountKey
        SignedPrivateKey.sign signedPrivateKey signKey.keyPairCsp
        |> PrivateEncryptionKey

    let validate (privateEncryptionKey : PrivateEncryptionKey)  =
        let (PrivateEncryptionKey signedPrivateKey) = privateEncryptionKey
        SignedPrivateKey.validate signedPrivateKey []

    let toSignedPrivateKey (PrivateEncryptionKey signedPrivateKey ) = signedPrivateKey
        
    let decrypt =
        toSignedPrivateKey >> SignedPrivateKey.toPrivateKey >> PrivateKey.toKeyPairsCsp >> KeyPairCsp.decrypt
      
    let toByteArray  =
        toSignedPrivateKey >> SignedPrivateKey.toPrivateKey >> PrivateKey.toKeyPairsCsp >> KeyPairCsp.toByteArray

type PublicEncryptionKey = private | PublicEncryptionKey of SignedPublicKey

module PublicEncryptionKey =
    
    let create (privateAccountKey: PrivateAccountKey) =
        PrivateEncryptionKey.toSignedPrivateKey
        >> SignedPublicKey.create 
        >> (fun )
module PrivateEncryptionKey =
    let private create' =
        PrivateKey.create >> PrivateEncryptionKey

    let private sign' (PrivateEncryptionKey privateKey) (PrivateAccountKey signKey) =
        let signedPrivateKey = PrivateKey.sign privateKey signKey.keyPairCsp
        PrivateEncryptionKey signedPrivateKey

    let create =
        create'() |> sign'
    let validate (PrivateEncryptionKey signedPrivateKey) (PublicAccountKey validateKey) =
        PrivateKey.validate signedPrivateKey [ validateKey ]


module PublicEncryptionKey =
    let create (PrivateEncryptionKey privateEncryptKey) (PrivateAccountKey signKey)  =
        let publicCsp = KeyPairCsp.toPublicCsp privateEncryptKey.keyPairCsp
        (PrivateAccountKey signKey)

    let sign (PrivateEncryptionKey privateKey) (PrivateAccountKey signKey) =
        let signedPrivateKey = PrivateKey.sign privateKey signKey.keyPairCsp
        PrivateEncryptionKey signedPrivateKey

    let validate  (PrivateEncryptionKey signedKey) (PublicAccountKey validateKey) =
        PrivateKey.validate signedKey [ validateKey ]


type PublicLoginKey = private | PublicLoginKey of PublicKey


type LoginKeyPair = private | LoginKeyPair of PrivateKey  
type SignatureKeyPair = private | SignatureKeyPair of PrivateKey  
type PrivateAccount =
 {
    encryptionKeyPair : EncryptionKeyPair
    loginKeyPair : LoginKeyPair
    signatureKeyPair: SignatureKeyPair
    accountKey: PrivateAccountKey
 }
 member x.AccountId = 
    let (PrivateAccountKey key) = x.accountKey
    let (PublicKeyId (KeyId identity)) = key.id.value
    identity

module PrivateAccount =
    let create() =
        

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
