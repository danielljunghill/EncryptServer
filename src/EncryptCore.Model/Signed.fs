namespace EncryptCore.Model

open EncryptCore.AssymetricEncryption

type Signed<'T> =
    {
        value:'T
        signature: ByteArraySignature     
    }

module Signed =
    let create map value keyPair  =
        {
            value = value 
            signature = Signature.Sign.byteArray256 keyPair (map value)
        }
    let validate map (signedValue: Signed<'T>)  =
        fun publicScp -> Signature.Verify.byteArray publicScp signedValue.signature (map signedValue.value)
