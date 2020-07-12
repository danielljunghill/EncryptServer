namespace EncryptCore.Model

open EncryptCore.AssymetricEncryption

type NontEmptyArray<'T> =
    {
        first: 'T
        rest: 'T []
    }

type Signed<'T> =
    {
        value:'T
        signature: NontEmptyArray<ByteArraySignature>    
    }

module Signed =
    let create map value keyPair  =
        {
            value = value 
            signature = Signature.Sign.byteArray256 keyPair (map value)
        }
    let validate map (signedValue: Signed<'T>)  =
        fun publicScp -> Signature.Verify.byteArray publicScp signedValue.signature (map signedValue.value)
    
    let sign 