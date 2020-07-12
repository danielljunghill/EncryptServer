namespace EncryptCore.Model
open EncryptCore.SymmetricEncryption
open EncryptCore
open EncryptCore.AssymetricEncryption

type Identity = private | Identity of byte[]

module Identity =
    let private create'  = 
            RandomByteArray.create
            >> RandomByteArray.toByteArray
          
    let create =
        fun () -> create'  128
        >> Identity

    let toBase64String (Identity bts) =
        Base64String.fromByteArray bts
    
    let fromBase64String =
        Base64String.toByteArray
        >> Identity

    let toByteArray (Identity bts) = bts
    
        

type ServerIdentity = private | ServerIdentity of Identity 
module ServerIdentity =
    let create =
        Identity.create >> ServerIdentity

type ClientIdentity = private | ClientIdentity of Identity
module ClientIdentity =
    let create =
         Identity.create >> ClientIdentity



