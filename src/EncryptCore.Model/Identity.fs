namespace EncryptCore.Model
open EncryptCore.SymmetricEncryption
open EncryptCore
open EncryptCore.AssymetricEncryption

type Identity = private | Identity of byte[]

module Identity =
    let private create'  = 
            RandomByteArray.create
            >> RandomByteArray.toByteArray
          
    let create count =
        fun () -> create'  count
        >> Identity

    let toBase64String (Identity bts) =
        Base64String.fromByteArray bts
    
    let fromBase64String =
        Base64String.toByteArray
        >> Identity

    let toByteArray (Identity bts) = bts
    
        



