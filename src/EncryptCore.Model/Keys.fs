namespace EncryptCore.Model
open EncryptCore.SymmetricEncryption
open EncryptCore.AssymetricEncryption
open EncryptCore
open EncryptCore.Hash
open Model.Signed
type KeyId = | KeyId of Identity


module FsharpHelper =
    let inparamReorder3 (fc: 'a -> 'b -> 'c) = fun b a -> fc a b
    let inverse2 =  inparamReorder3   
    let (y_x) = inverse2
    let inparamReorder4 (fc: 'a -> 'b -> 'c -> 'd) = fun c a b  -> fc a b c
    let inverse3 =  inparamReorder4 
    let (z_x_y) = inverse3
    let f3map (fc: 'a -> 'b -> 'c) fm = fun a b -> (fc a b) |> fm
    let (>>-) = f3map

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
    let isValid =
        toPublicKey 

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
    let validate (signedPublicIdKey: SignedPublicIdKey) =
        let signedValue = SignedPublicIdKey.toSignedValue signedPublicIdKey
        let signedOwnPublicKey = SignedPublicIdKey.toPublicKey signedPublicIdKey
        toPublicKey 
        >> (fun publicKey -> Signed.validate PublicIdKey.toByteArray signedValue [signedOwnPublicKey ; publicKey ] )
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
    let isValid  =
        toSignedPublicIdKey 
        >>  fun signedPublicKey -> PublicAccountKey.validate signedPublicKey
    let encrypt =
        toPublicKey
        >> PublicKey.encrypt


type PrivateAccountLoginKey = private | PrivateAccountLoginKey of PrivateAccountMemberKey
module PrivateAccountLoginKey =
    let create = PrivateAccountMemberKey.create >> PrivateAccountLoginKey
    let toPrivateAccountMemberKey (PrivateAccountLoginKey privateAccountMemberKey) = privateAccountMemberKey
    let isValid = toPrivateAccountMemberKey >> PrivateAccountMemberKey.isValid
    let decrypt = toPrivateAccountMemberKey >> PrivateAccountMemberKey.decrypt
    let sign<'T>= toPrivateAccountMemberKey >> PrivateAccountMemberKey.sign<'T> 
    let resign<'T> = toPrivateAccountMemberKey >> PrivateAccountMemberKey.resign<'T>


type PublicAccountLoginKey = private | PublicAccountLoginKey of PublicAccountMemberKey
module PublicAccountLoginKey =
    let toPublicAccountMemberKey (PublicAccountLoginKey publicAccountMemberKey) = publicAccountMemberKey
    let create = 
        PrivateAccountLoginKey.toPrivateAccountMemberKey >> PublicAccountMemberKey.create >>- PublicAccountLoginKey
    let isValid = toPublicAccountMemberKey >> PublicAccountMemberKey.isValid
    let encrypt = toPublicAccountMemberKey >> PublicAccountMemberKey.encrypt  


type PrivateAccountEncryptionKey = private | PrivateAccountEncryptionKey of PrivateAccountMemberKey
module PrivateAccountEncryptionKey =
    let create = PrivateAccountMemberKey.create >> PrivateAccountEncryptionKey
    let toPrivateAccountMemberKey (PrivateAccountEncryptionKey privateAccountEncryptionKey) = privateAccountEncryptionKey
    let isValid = toPrivateAccountMemberKey >> PrivateAccountMemberKey.isValid
    let decrypt = toPrivateAccountMemberKey >> PrivateAccountMemberKey.decrypt
    let sign<'T>= toPrivateAccountMemberKey >> PrivateAccountMemberKey.sign<'T> 
    let resign<'T> = toPrivateAccountMemberKey >> PrivateAccountMemberKey.resign<'T>


type PublicAccountEncryptionKey  = private | PublicAccountEncryptionKey  of PublicAccountMemberKey
module PublicAccountEncryptionKey  =
    let toPublicAccountMemberKey (PublicAccountEncryptionKey publicAccountEncryptionKey) = publicAccountEncryptionKey
    let create = 
        PrivateAccountEncryptionKey.toPrivateAccountMemberKey >> PublicAccountMemberKey.create >>- PublicAccountEncryptionKey
    let isValid = toPublicAccountMemberKey >> PublicAccountMemberKey.isValid
    let encrypt = toPublicAccountMemberKey >> PublicAccountMemberKey.encrypt  


type PrivateAccountSignKey = private | PrivateAccountSignKey of PrivateAccountMemberKey
module PrivateAccountSignKey =
    let create = PrivateAccountMemberKey.create >> PrivateAccountSignKey
    let toPrivateAccountMemberKey (PrivateAccountSignKey privateAccountEncryptionKey) = privateAccountEncryptionKey
    let isValid = toPrivateAccountMemberKey >> PrivateAccountMemberKey.isValid
    let sign<'T>= toPrivateAccountMemberKey >> PrivateAccountMemberKey.sign<'T> 
    let resign<'T> = toPrivateAccountMemberKey >> PrivateAccountMemberKey.resign<'T>


type PublicAccountSignKey  = private | PublicAccountSignKey  of PublicAccountMemberKey
module PublicAccountSignKeyy  =
    let toPublicAccountMemberKey (PublicAccountSignKey publicAccountEncryptionKey) = publicAccountEncryptionKey
    let create = 
        PrivateAccountSignKey.toPrivateAccountMemberKey >> PublicAccountMemberKey.create >>- PublicAccountSignKey
    let isValid = toPublicAccountMemberKey >> PublicAccountMemberKey.isValid
    let encrypt = toPublicAccountMemberKey >> PublicAccountMemberKey.encrypt  



  


