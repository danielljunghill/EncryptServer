namespace EncryptCore.Model

open EncryptCore.AssymetricEncryption

type NotEmptyArray<'T> =
    {
        first: 'T
        rest: 'T []
    }
module NotEmptyArray =
    let create first =
        {
            first = first
            rest = Array.empty
        }
    let add nextValue (nea: NotEmptyArray<_>)   =
        { nea with rest = Array.append nea.rest [|nextValue|] }
    let last (nea: NotEmptyArray<_>) =
        match nea.rest.Length with
        | len when len = 0 -> nea.first
        | _ -> nea.rest.[nea.rest.Length - 1]
    let tryBeforeLast (nea: NotEmptyArray<_>) =  
        match nea.rest.Length with
        | len when len = 0 -> None
        | len when len = 1 -> nea.first
        | _ -> nea.rest.[nea.rest.Length - 2]

            

type Signed<'T> =
    {
        value:'T
        signatures: NotEmptyArray<ByteArraySignature>    
    }

module Signed =
    let create map value keyPair  =
        {
            value = value 
            signatures =   Signature.Sign.byteArray256 keyPair (map value) |> NotEmptyArray.create
        }
    let resign (signedValue:Signed<'T>) keyPair =
        let btsToSign = NotEmptyArray.last signedValue.signatures |> ByteArraySignature.toByteArray 
        let signature = Signature.Sign.byteArray256 keyPair btsToSign
        { signedValue with signatures = NotEmptyArray.add signature signedValue.signatures }  

    let validate (signedValue:Signed<'T>) publicCps =
        let signature = 
        type Signed<'T> =
            {
                value:'T
                signatures: NotEmptyArray<ByteArraySignature>    
            }
    //let validate map (signedValue: Signed<'T>)  =
    //    fun publicScp -> Signature.Verify.byteArray publicScp signedValue.signature (map signedValue.value)
    
    //let sign 