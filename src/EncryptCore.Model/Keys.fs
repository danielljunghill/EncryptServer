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

    let toSignedPrivateIdKey (SignedPrivateIdKey signedPrivateIdKey) = signedPrivateIdKey

    let toPrivateIdKey  = toSignedPrivateIdKey >> (fun signedKey -> signedKey.value)

    let toPrivateKey = toPrivateIdKey >> (fun signedKey -> signedKey.privateKey)

    let toId = toPrivateIdKey >> (fun signedKey -> signedKey.id)

    let resign<'T> (signed:Signed<'T>) (SignedPrivateIdKey signedKey) =
        Signed.sign signed signedKey.value.privateKey

    let sign map value =     
        toPrivateKey >>
        Signed.createAndSign map value 

    let isValid  =
        toSignedPrivateIdKey
        >> fun value -> Signed.validate PrivateIdKey.toByteArray value

module SignedPublicIdKey =

    let create signedPrivateIdKey =
        //check if private key is valid
        let publicKeyId = SignedPrivateIdKey.toPrivateIdKey signedPrivateIdKey  |> PublicIdKey.create 
        SignedPrivateIdKey.sign PublicIdKey.toByteArray publicKeyId signedPrivateIdKey
        |> SignedPublicIdKey

    let toSignedValue (SignedPublicIdKey signedKey) = signedKey

    let toPublicIdKey = toSignedValue >> fun v -> v.value

    let toPublicKey = toSignedValue >> fun v -> v.value.publicKey

    let toId = toSignedValue >> fun v -> v.value.id
  
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

    let resign value  =
        toSignedPrivateIdKey >> SignedPrivateIdKey.resign value 

    let sign map value =
        toSignedPrivateIdKey >> SignedPrivateIdKey.sign map value

    let decrypt =
        toPrivateKey >> PrivateKey.decrypt


module PublicAccountKey =

    let create =
        PrivateAccountKey.toSignedPrivateIdKey
        >> SignedPublicIdKey.create
        >> PublicAccountKey

    let toSignedPublicIdKey (PublicAccountKey publicAccountIdKey) = publicAccountIdKey

    let toSignedValue = toSignedPublicIdKey >> SignedPublicIdKey.toSignedValue

    let toPublicIdKey = toSignedPublicIdKey >> SignedPublicIdKey.toPublicIdKey

    let toPublicKey = toSignedPublicIdKey >> SignedPublicIdKey.toPublicKey

    let toId = toSignedPublicIdKey >> SignedPublicIdKey.toPublicKey

    let isValid (publicAccountKey: PublicAccountKey) =
        PrivateAccountKey.toPublicKey
        >> (fun publicKey -> Signed.validate PublicIdKey.toByteArray (toSignedValue publicAccountKey) [publicKey])

    let validate (signedPublicIdKey: SignedPublicIdKey) =
        let signedValue = SignedPublicIdKey.toSignedValue signedPublicIdKey
        let signedOwnPublicKey = SignedPublicIdKey.toPublicKey signedPublicIdKey
        toPublicKey 
        >> (fun publicKey -> Signed.validate PublicIdKey.toByteArray signedValue [signedOwnPublicKey ; publicKey ] )

type PrivateEncryptionKey = private | PrivateEncryptionKey of SignedPrivateIdKey

module PrivateEncryptionKey =

    let create  =
        let signedPrivateIdKey = SignedPrivateIdKey.create() |> SignedPrivateIdKey.toSignedPrivateIdKey
        PrivateAccountKey.resign signedPrivateIdKey 
        >> SignedPrivateIdKey
        >> PrivateEncryptionKey

    let toSignedPrivateIdKey (PrivateEncryptionKey privateEncryptionKey) =  privateEncryptionKey

    let toSignedValue = toSignedPrivateIdKey >> SignedPrivateIdKey.toSignedPrivateIdKey

    let toPrivateKey = 
        toSignedPrivateIdKey
        >> SignedPrivateIdKey.toPrivateKey

    let toPublicKey = 
        toSignedPrivateIdKey
        >> SignedPrivateIdKey.toPrivateKey
        >> PrivateKey.toPublicKey
    
    let decrypt =
        toPrivateKey >> PrivateKey.decrypt

    let isValid (privateEncryptionKey:  PrivateEncryptionKey) =
        let signed = toSignedValue privateEncryptionKey
        let signedPublicKey = toPublicKey privateEncryptionKey
        PublicAccountKey.toPublicKey
        >> fun accountPublicKey -> Signed.validate PrivateIdKey.toByteArray signed [signedPublicKey; accountPublicKey ]
    

type PublicEncryptionKey = private | PublicEncryptionKey of SignedPublicIdKey

module PublicEncryptionKey =
    
    let create (privateAccountKey: PrivateAccountKey) =
        PrivateEncryptionKey.toSignedPrivateIdKey
        >> SignedPublicIdKey.create 
        >> SignedPublicIdKey.toSignedValue
        >> (fun signedKey -> PrivateAccountKey.resign signedKey privateAccountKey)
        >> SignedPublicIdKey
        >> PublicEncryptionKey
     
    let toSignedPublicIdKey (PublicEncryptionKey signedPublicIdKey) = signedPublicIdKey

    let toSignedValue = toSignedPublicIdKey >> SignedPublicIdKey.toSignedValue

    let toPublicIdKey = toSignedPublicIdKey >> SignedPublicIdKey.toPublicIdKey

    let toPublicKey = toSignedPublicIdKey >> SignedPublicIdKey.toPublicKey

    let isValid  =
        toSignedPublicIdKey 
        >>  fun signedPublicKey -> PublicAccountKey.validate signedPublicKey

    let encrypt =
        toPublicKey
        >> PublicKey.encrypt
    


type PrivateSignKey = private | PrivateSignKey of SignedPrivateIdKey

module PrivateSignKey =

    let create  =
        let signedPrivateIdKey = SignedPrivateIdKey.create() |> SignedPrivateIdKey.toSignedPrivateIdKey
        PrivateAccountKey.resign signedPrivateIdKey 
        >> SignedPrivateIdKey
        >> PrivateSignKey

    let toSignedPrivateIdKey (PrivateSignKey privateEncryptionKey) =  privateEncryptionKey

    let toSignedValue = toSignedPrivateIdKey >> SignedPrivateIdKey.toSignedPrivateIdKey

    let toPrivateKey = 
        toSignedPrivateIdKey
        >> SignedPrivateIdKey.toPrivateKey

    let toPublicKey = 
        toSignedPrivateIdKey
        >> SignedPrivateIdKey.toPrivateKey
        >> PrivateKey.toPublicKey
    
    let isValid (privateEncryptionKey:  PrivateSignKey) =
        let signed = toSignedValue privateEncryptionKey
        let signedPublicKey = toPublicKey privateEncryptionKey
        PublicAccountKey.toPublicKey
        >> fun accountPublicKey -> Signed.validate PrivateIdKey.toByteArray signed [signedPublicKey; accountPublicKey ]

    let resign value  =
        toSignedPrivateIdKey >> SignedPrivateIdKey.resign value 

    let sign map value =
        toSignedPrivateIdKey >> SignedPrivateIdKey.sign map value

    

type PublicEncryptionKey = private | PublicEncryptionKey of SignedPublicIdKey

module PublicEncryptionKey =
    
    let create (privateAccountKey: PrivateAccountKey) =
        PrivateEncryptionKey.toSignedPrivateIdKey
        >> SignedPublicIdKey.create 
        >> SignedPublicIdKey.toSignedValue
        >> (fun signedKey -> PrivateAccountKey.resign signedKey privateAccountKey)
        >> SignedPublicIdKey
        >> PublicEncryptionKey
     
    let toSignedPublicIdKey (PublicEncryptionKey signedPublicIdKey) = signedPublicIdKey

    let toSignedValue = toSignedPublicIdKey >> SignedPublicIdKey.toSignedValue

    let toPublicIdKey = toSignedPublicIdKey >> SignedPublicIdKey.toPublicIdKey

    let toPublicKey = toSignedPublicIdKey >> SignedPublicIdKey.toPublicKey

    let isValid  =
        toSignedPublicIdKey 
        >>  fun signedPublicKey -> PublicAccountKey.validate signedPublicKey

    let encrypt =
        toPublicKey
        >> PublicKey.encrypt


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
