namespace EncryptCore.Model
open EncryptCore.SymmetricEncryption
open EncryptCore
type Identity = private | Identity of string

module Identity =
    let private create'  = 
            RandomByteArray.create
            >> RandomByteArray.toByteArray
            >> ByteArray.toBase64String
            >> Identity  
    let create =
        create'  128

type ServerIdentity = private | ServerIdentity of Identity 
module ServerIdentity =
    let create =
        Identity.create

type ClientIdentity = private | ClientIdentity of Identity
module ClientIdentity =
    let create =
         Identity.create |> ClientIdentity
