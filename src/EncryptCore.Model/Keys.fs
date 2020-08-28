namespace EncryptCore.Model
open EncryptCore.SymmetricEncryption
open EncryptCore.AssymetricEncryption
open EncryptCore
open EncryptCore.Hash
open Model.Signed
type KeyId = | KeyId of Identity


module FsharpHelper =
    let pmove2 (fc: 'a -> 'b -> 'c) = fun b a -> fc a b 
    let (y_x) = pmove2
    let pmove3 (fc: 'a -> 'b -> 'c -> 'd) = fun c a b  -> fc a b c
    let (z_x_y) = pmove3
    let fmap3 (fc: 'a -> 'b -> 'c) fm = fun a b -> (fc a b) |> fm
    let (>>-) = fmap3
    let mapSecond f (fc: 'a -> 'b -> 'c -> 'd)  = fun  a b c  -> fc a (f b) c 

open FsharpHelper
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
    let toPublicKey (privateIdKey: PrivateIdKey) = PrivateKey.toPublicKey privateIdKey.privateKey

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
        let signed = Signed.createAndSign signKey PrivateIdKey.toByteArray privateIdKey 
        SignedPrivateIdKey signed
    let create =
        PrivateIdKey.create
        >> fromPrivateIdKey
    let toSignedPrivateIdKey (SignedPrivateIdKey signedPrivateIdKey) = signedPrivateIdKey
    let toPrivateIdKey  = toSignedPrivateIdKey >> (fun signedKey -> signedKey.value)
    let toPrivateKey = toPrivateIdKey >> (fun signedKey -> signedKey.privateKey)
    let toId = toPrivateIdKey >> (fun signedKey -> signedKey.id)
    let resign<'T> (signed:Signed<'T>) (SignedPrivateIdKey signedKey) =
        Signed.sign signedKey.value.privateKey signed 
    let sign map value =     
        toPrivateKey >>
        fun privateKey -> Signed.createAndSign privateKey map value 
    let isValid  =
        toSignedPrivateIdKey
        >> fun value -> Signed.validate PrivateIdKey.toByteArray value [ PrivateIdKey.toPublicKey value.value ]

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



type PrivateAccountKey = private | PrivateAccountKey of SignedPrivateIdKey
module PrivateAccountKey =

    let create =
        SignedPrivateIdKey.create
        >> PrivateAccountKey
    let toSignedPrivateIdKey (PrivateAccountKey signedPrivateIdKey) = signedPrivateIdKey
    let toPrivateIdKey =  toSignedPrivateIdKey >> SignedPrivateIdKey.toPrivateIdKey
    let toSignedValue = toSignedPrivateIdKey >> SignedPrivateIdKey.toSignedPrivateIdKey
    let toPrivateKey = toSignedPrivateIdKey >> SignedPrivateIdKey.toPrivateKey
    let toPublicKey =
        toPrivateKey >> PrivateKey.toPublicKey
    let resign value  =
        toSignedPrivateIdKey >> SignedPrivateIdKey.resign value 
    let sign map value =
        toSignedPrivateIdKey >> SignedPrivateIdKey.sign map value
    let decrypt =
        toPrivateKey >> PrivateKey.decrypt
    let isValid  =
        toSignedPrivateIdKey >>
        fun signedPrivateIdKey ->
            let signedIdKey = signedPrivateIdKey |> SignedPrivateIdKey.toSignedPrivateIdKey
            let publicKey = signedPrivateIdKey |> SignedPrivateIdKey.toPrivateKey |>  PrivateKey.toPublicKey 
            Signed.validate PrivateIdKey.toByteArray signedIdKey [ publicKey ]

type PublicAccountKey = private | PublicAccountKey of SignedPublicIdKey
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
    //let validate (signedPublicIdKey: SignedPublicIdKey) =
    //    let signedValue = SignedPublicIdKey.toSignedValue signedPublicIdKey
    //    let signedOwnPublicKey = SignedPublicIdKey.toPublicKey signedPublicIdKey
    //    toPublicKey 
    //    >> (fun publicKey -> Signed.validate PublicIdKey.toByteArray signedValue [signedOwnPublicKey ; publicKey ] )
    let encrypt =
        toPublicKey
        >> PublicKey.encrypt


type PrivateAccountMemberKey = private | PrivateAccountMemberKey of SignedPrivateIdKey
module PrivateAccountMemberKey =
    let create  =
        let signedPrivateIdKey = SignedPrivateIdKey.create() |> SignedPrivateIdKey.toSignedPrivateIdKey
        PrivateAccountKey.resign signedPrivateIdKey 
        >> SignedPrivateIdKey
        >> PrivateAccountMemberKey

    let toSignedPrivateIdKey (PrivateAccountMemberKey privateEncryptionKey) =  privateEncryptionKey
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
    let isValid (privateEncryptionKey:  PrivateAccountMemberKey) =
        let signed = toSignedValue privateEncryptionKey
        let signedPublicKey = toPublicKey privateEncryptionKey
        PublicAccountKey.toPublicKey
        >> fun accountPublicKey -> Signed.validate PrivateIdKey.toByteArray signed [signedPublicKey; accountPublicKey ] 
    let resign<'T> =  
        fun value -> toSignedPrivateIdKey >> SignedPrivateIdKey.resign<'T> value
        |> y_x
    let sign<'T> = 
         fun map value -> toSignedPrivateIdKey >> SignedPrivateIdKey.sign map (value: 'T)
         |> z_x_y
 
type PublicAccountMemberKey  = private | PublicAccountMemberKey of SignedPublicIdKey
module PublicAccountMemberKey =
    
    let private create' (privateAccountKey: PrivateAccountKey) =
        PrivateAccountMemberKey.toSignedPrivateIdKey
        >> SignedPublicIdKey.create 
        >> SignedPublicIdKey.toSignedValue
        >> (fun signedKey -> PrivateAccountKey.resign signedKey privateAccountKey)
        >> SignedPublicIdKey
        >> PublicAccountMemberKey

    let create =  create' |> y_x
    let toSignedPublicIdKey (PublicAccountMemberKey signedPublicIdKey) = signedPublicIdKey
    let toSignedValue = toSignedPublicIdKey >> SignedPublicIdKey.toSignedValue
    let toPublicIdKey = toSignedPublicIdKey >> SignedPublicIdKey.toPublicIdKey
    let toPublicKey = toSignedPublicIdKey >> SignedPublicIdKey.toPublicKey
    let isValid publicAccountMemberKey privateAccountMemberKey publicAccountKey =
        let signedValue = toSignedValue publicAccountMemberKey
        let signMemberKey = PrivateAccountMemberKey.toPublicKey privateAccountMemberKey
        let signPublicAccountKey = PublicAccountKey.toPublicKey publicAccountKey
        Signed.validate PublicIdKey.toByteArray signedValue [signMemberKey ; signPublicAccountKey]
    let encrypt =
        toPublicKey
        >> PublicKey.encrypt


type PrivateLoginKey = private | PrivateLoginKey of PrivateAccountMemberKey
module PrivateLoginKey =
    let create = PrivateAccountMemberKey.create >> PrivateLoginKey
    let toPrivateAccountMemberKey (PrivateLoginKey privateAccountMemberKey) = privateAccountMemberKey
    let isValid = toPrivateAccountMemberKey >> PrivateAccountMemberKey.isValid
    let decrypt = toPrivateAccountMemberKey >> PrivateAccountMemberKey.decrypt
    let sign<'T>= toPrivateAccountMemberKey >> PrivateAccountMemberKey.sign<'T> 
    let resign<'T> = toPrivateAccountMemberKey >> PrivateAccountMemberKey.resign<'T>


type PublicLoginKey = private | PublicLoginKey of PublicAccountMemberKey
module PublicLoginKey =
    let toPublicAccountMemberKey (PublicLoginKey publicAccountMemberKey) = publicAccountMemberKey
    let create = 
        PrivateLoginKey.toPrivateAccountMemberKey >> PublicAccountMemberKey.create >>- PublicLoginKey
    let isValid  = mapSecond PrivateLoginKey.toPrivateAccountMemberKey  (toPublicAccountMemberKey >> PublicAccountMemberKey.isValid)
    let encrypt = toPublicAccountMemberKey >> PublicAccountMemberKey.encrypt  


type PrivateEncryptionKey = private | PrivateEncryptionKey of PrivateAccountMemberKey
module PrivateEncryptionKey =
    let create = PrivateAccountMemberKey.create >> PrivateEncryptionKey
    let toPrivateAccountMemberKey (PrivateEncryptionKey privateAccountEncryptionKey) = privateAccountEncryptionKey
    let isValid = toPrivateAccountMemberKey >> PrivateAccountMemberKey.isValid
    let decrypt = toPrivateAccountMemberKey >> PrivateAccountMemberKey.decrypt
    let sign<'T>= toPrivateAccountMemberKey >> PrivateAccountMemberKey.sign<'T> 
    let resign<'T> = toPrivateAccountMemberKey >> PrivateAccountMemberKey.resign<'T>


type PublicEncryptionKey  = private | PublicEncryptionKey  of PublicAccountMemberKey
module PublicEncryptionKey  =
    let toPublicAccountMemberKey (PublicEncryptionKey publicAccountEncryptionKey) = publicAccountEncryptionKey
    let create = 
        PrivateEncryptionKey.toPrivateAccountMemberKey >> PublicAccountMemberKey.create >>- PublicEncryptionKey
    let isValid =  mapSecond PrivateEncryptionKey.toPrivateAccountMemberKey (toPublicAccountMemberKey >> PublicAccountMemberKey.isValid)
    let encrypt = toPublicAccountMemberKey >> PublicAccountMemberKey.encrypt  


type PrivateSignKey = private | PrivateSignKey of PrivateAccountMemberKey
module PrivateSignKey =
    let create = PrivateAccountMemberKey.create >> PrivateSignKey
    let toPrivateAccountMemberKey (PrivateSignKey privateAccountEncryptionKey) = privateAccountEncryptionKey
    let isValid = toPrivateAccountMemberKey >> PrivateAccountMemberKey.isValid
    let sign<'T>= toPrivateAccountMemberKey >> PrivateAccountMemberKey.sign<'T> 
    let resign<'T> = toPrivateAccountMemberKey >> PrivateAccountMemberKey.resign<'T>


type PublicSignKey  = private | PublicSignKey  of PublicAccountMemberKey
module PublicSignKey  =
    let toPublicAccountMemberKey (PublicSignKey publicAccountEncryptionKey) = publicAccountEncryptionKey
    let create = 
        PrivateSignKey.toPrivateAccountMemberKey >> PublicAccountMemberKey.create >>- PublicSignKey
    let isValid = mapSecond PrivateSignKey.toPrivateAccountMemberKey  (toPublicAccountMemberKey >> PublicAccountMemberKey.isValid)
    let encrypt = toPublicAccountMemberKey >> PublicAccountMemberKey.encrypt  
    

type PrivateAccount =
    {
        accountKey: PrivateAccountKey
        loginKey: PrivateLoginKey
        encryptionKey: PrivateEncryptionKey
        signKey: PrivateSignKey
    }

type PublicAccount =
    {
        accountKey: PublicAccountKey
        loginKey: PublicLoginKey
        encryptionKey: PublicEncryptionKey
        signKey: PublicSignKey
    }
    
module  PrivateAccount =
    
    let create() =
        let privateAccountKey = PrivateAccountKey.create()
        {
            PrivateAccount.accountKey = privateAccountKey
            loginKey = PrivateLoginKey.create privateAccountKey
            encryptionKey = PrivateEncryptionKey.create privateAccountKey
            signKey =  PrivateSignKey.create privateAccountKey
        }

    let isValid (privateAccount: PrivateAccount)   =
        let publicKey = PublicAccountKey.create privateAccount.accountKey
        PrivateAccountKey.isValid privateAccount.accountKey
        && PrivateEncryptionKey.isValid privateAccount.encryptionKey publicKey
        && PrivateLoginKey.isValid privateAccount.loginKey publicKey
        && PrivateSignKey.isValid privateAccount.signKey publicKey


    let toPublicAccount (privateAccount: PrivateAccount) =
        let publicAccountKey = PublicAccountKey.create privateAccount.accountKey
        let privateAccountKey = privateAccount.accountKey
        {
            PublicAccount.accountKey = publicAccountKey
            loginKey = PublicLoginKey.create privateAccount.loginKey privateAccountKey
            encryptionKey = PublicEncryptionKey.create privateAccount.encryptionKey privateAccountKey
            signKey =  PublicSignKey.create privateAccount.signKey privateAccountKey
        }


module PublicAccount =

    let isValid (publicAccount : PublicAccount) (privateAccount : PrivateAccount) =
        let privateAccountKey = privateAccount.accountKey
        let publicAccountKey = publicAccount.accountKey 
        PublicAccountKey.isValid publicAccountKey privateAccountKey
        && PublicEncryptionKey.isValid publicAccount.encryptionKey privateAccount.encryptionKey publicAccountKey
        && PublicLoginKey.isValid publicAccount.loginKey privateAccount.loginKey publicAccountKey
        && PublicSignKey.isValid publicAccount.signKey privateAccount.signKey publicAccountKey
        





  


