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


type PrivateIdKey =
    {
        id: KeyId
        privateKey:PrivateKey
    }

module PrivateIdKey =
    let toByteArray (privateIdKey : PrivateIdKey) =
        Array.append (KeyId.toByteArray privateIdKey.id) (PrivateKey.toByteArray privateIdKey.privateKey)   
    let toPrivateKey (privateIdKey: PrivateIdKey) = privateIdKey.privateKey
    let create() =
        { id = KeyId.create(); privateKey = PrivateKey.create() }
    let toPublicCsp (privateIdKey: PrivateIdKey) = PrivateKey.toPublicKey privateIdKey.privateKey

type PublicIdKey =
    {
        id: KeyId
        publicKey: PublicKey
    }

module PublicIdKey =
    let toByteArray (publicIdKey : PublicIdKey) =
        Array.append (KeyId.toByteArray publicIdKey.id) (PublicKey.toByteArray publicIdKey.publicKey)
    let create (privateIdKey: PrivateIdKey) =
        { id = privateIdKey.id; publicKey = PrivateKey.toPublicKey privateIdKey.privateKey }

type SignedPrivateIdKey = private | SignedPrivateIdKey of Signed<PrivateIdKey>
type SignedPublicIdKey = private | SignedPublicIdKey of Signed<PublicIdKey>


module SignedPrivateIdKey =

    let fromPrivateIdKey (privateIdKey: PrivateIdKey)  =
        let signKey =  privateIdKey.privateKey
        let signed = Signed.createAndSign PrivateIdKey.toByteArray privateIdKey signKey
        SignedPrivateIdKey signed

    let create =
        PrivateIdKey.create
        >> fromPrivateIdKey

    let toPrivateIdKey (SignedPrivateIdKey signedKey) = signedKey.value

    let toPrivateKey = toPrivateIdKey >> (fun signedKey -> signedKey.privateKey)

    let toId = toPrivateIdKey >> (fun signedKey -> signedKey.id)

    let resign<'T> (signed:Signed<'T>) (SignedPrivateIdKey signedKey) =
        Signed.sign signed signedKey.value.privateKey

    let sign map value =     
        toPrivateKey >>
        Signed.createAndSign map value 

module SignedPublicIdKey =

    let create signedPrivateIdKey =
        //check if private key is valid
        let publicKeyId = SignedPrivateIdKey.toPrivateIdKey signedPrivateIdKey  |> PublicIdKey.create 
        SignedPrivateIdKey.sign PublicIdKey.toByteArray publicKeyId signedPrivateIdKey
        |> SignedPublicIdKey

    let toSignedKey (SignedPublicIdKey signedKey) = signedKey

    let toPublicIdKey = toSignedKey >> fun v -> v.value

    let toPublicKey = toSignedKey >> fun v -> v.value.publicKey

    let toId = toSignedKey >> fun v -> v.value.id
  
    let validate map signed (signedPublicIdKey: SignedPublicIdKey list) =
        Signed.validate map signed (signedPublicIdKey |> List.map toPublicKey)

type PublicAccountKey = private | PublicAccountKey of SignedPublicIdKey
type PrivateAccountKey = private | PrivateAccountKey of SignedPrivateIdKey

module PrivateAccountKey =

    let create =
        SignedPrivateIdKey.create
        >> PrivateAccountKey


    let toSignedPrivateIdKey (PrivateAccountKey signedPrivateIdKey) = signedPrivateIdKey

    let toPrivateIdKey =  toSignedPrivateIdKey >> SignedPrivateIdKey.toPrivateIdKey

    let toPrivateKey = toSignedPrivateIdKey >> SignedPrivateIdKey.toPrivateKey

    let toPublicKey =
        toPrivateKey >> PrivateKey.toPublicKey

    //let validate (PrivateAccountKey signedPrivateIdKey)  =
    //    SignedPublicIdKey.va signedPrivateIdKey []
    //let toSignedPrivateIdKey (PrivateAccountKey PrivateIdKey) = PrivateIdKey
    //let toSignKey (PrivateAccountKey PrivateIdKey) = SignedPrivateIdKey.toPrivateIdKey PrivateIdKey
    //let sign<'T> (signed: Signed<'T>) = 
    //    toSignedPrivateIdKey
    //    >> SignedPrivateIdKey.sign

module PublicAccountKey =

    let create =
        PrivateAccountKey.toSignedPrivateIdKey
        >> SignedPublicIdKey.create
        >> PublicAccountKey
    let toSignedPublicIdKey (PublicAccountKey publicAccountKey) = publicAccountKey
    let validate (PublicAccountKey publicAccountKey)  =
        SignedPublicIdKey.validate publicAccountKey []

type PrivateEncryptionKey = private | PrivateEncryptionKey of SignedPrivateIdKey

module PrivateEncryptionKey =

    let create (privateAccountKey: PrivateAccountKey) =
        let signedPrivateIdKey = SignedPrivateIdKey.create()
        let signKey = PrivateAccountKey.toSignKey privateAccountKey
        SignedPrivateIdKey.sign signedPrivateIdKey signKey.keyPairCsp
        |> PrivateEncryptionKey

    let validate (privateEncryptionKey : PrivateEncryptionKey)  =
        let (PrivateEncryptionKey signedPrivateIdKey) = privateEncryptionKey
        SignedPrivateIdKey.validate signedPrivateIdKey []

    let toSignedPrivateIdKey (PrivateEncryptionKey signedPrivateIdKey ) = signedPrivateIdKey
        
    let decrypt =
        toSignedPrivateIdKey >> SignedPrivateIdKey.toPrivateIdKey >> PrivateIdKey.toKeyPairsCsp >> KeyPairCsp.decrypt
      
    let toByteArray  =
        toSignedPrivateIdKey >> SignedPrivateIdKey.toPrivateIdKey >> PrivateIdKey.toKeyPairsCsp >> KeyPairCsp.toByteArray

type PublicEncryptionKey = private | PublicEncryptionKey of SignedPublicIdKey

module PublicEncryptionKey =
    
    let create (privateAccountKey: PrivateAccountKey) =
        PrivateEncryptionKey.toSignedPrivateIdKey
        >> SignedPublicIdKey.create 
        >> (fun )
module PrivateEncryptionKey =
    let private create' =
        PrivateIdKey.create >> PrivateEncryptionKey

    let private sign' (PrivateEncryptionKey PrivateIdKey) (PrivateAccountKey signKey) =
        let signedPrivateIdKey = PrivateIdKey.sign PrivateIdKey signKey.keyPairCsp
        PrivateEncryptionKey signedPrivateIdKey

    let create =
        create'() |> sign'
    let validate (PrivateEncryptionKey signedPrivateIdKey) (PublicAccountKey validateKey) =
        PrivateIdKey.validate signedPrivateIdKey [ validateKey ]


module PublicEncryptionKey =
    let create (PrivateEncryptionKey privateEncryptKey) (PrivateAccountKey signKey)  =
        let publicCsp = KeyPairCsp.toPublicCsp privateEncryptKey.keyPairCsp
        (PrivateAccountKey signKey)

    let sign (PrivateEncryptionKey PrivateIdKey) (PrivateAccountKey signKey) =
        let signedPrivateIdKey = PrivateIdKey.sign PrivateIdKey signKey.keyPairCsp
        PrivateEncryptionKey signedPrivateIdKey

    let validate  (PrivateEncryptionKey signedKey) (PublicAccountKey validateKey) =
        PrivateIdKey.validate signedKey [ validateKey ]


type PublicLoginKey = private | PublicLoginKey of PublicIdKey


type LoginKeyPair = private | LoginKeyPair of PrivateIdKey  
type SignatureKeyPair = private | SignatureKeyPair of PrivateIdKey  
type PrivateAccount =
 {
    encryptionKeyPair : EncryptionKeyPair
    loginKeyPair : LoginKeyPair
    signatureKeyPair: SignatureKeyPair
    accountKey: PrivateAccountKey
 }
 member x.AccountId = 
    let (PrivateAccountKey key) = x.accountKey
    let (PublicIdKeyId (KeyId identity)) = key.id.value
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
//    encryptionPublicIdKey: PublicEncryptionKey
//    loginPublicIdKey : PublicLoginKey
// }

// module private PublicAccount =
//     let fromPrivate (account: PrivateAccount) =
//        let result =
//            {
//                identity = account.identity
//                encryptionPublicIdKey = PublicEncryptionKey.extract account.encryptionKeyPair
//                loginPublicIdKey = PublicLoginKey.extract account.loginKeyPair
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
