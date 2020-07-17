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
    let toArray (nea: NotEmptyArray<_>) = Array.append [| nea.first |] nea.rest
    let toList = toArray >> Array.toList

           
type Signed<'T> =
    {
        value:'T
        signatures: NotEmptyArray<ByteArraySignature>    
    }

module Signed =
    type SignatureWithByteArray =
        {
            signedByteArray: byte[]
            signature: ByteArraySignature
        }
    module SignatureWithByteArray =

        let verify (swba:SignatureWithByteArray)  publicCps =
            Signature.Verify.byteArray publicCps swba.signature swba.signedByteArray

        let getListFromSigned map (signedValue: Signed<_>) =
            let rec getListFromSignatures signedByteArrays =
                match signedByteArrays with
                | head :: [] -> 
                    [ { signedByteArray = signedValue.value |> map ; signature = head } ]
                | head :: tail ->
                    { signedByteArray = ByteArraySignature.toByteArray tail.Head ; signature = head } :: getListFromSignatures tail
                | [] -> []
            getListFromSignatures (NotEmptyArray.toList signedValue.signatures)

     //create Signed and add first signature of original value            
    let createAndSign map value keyPair  =
        {
            value = value 
            signatures =   Signature.Sign.byteArray256 keyPair (map value) |> NotEmptyArray.create
        }

    let sign (signedValue:Signed<'T>) keyPair =
        //take bytearray for last signature
        let btsToSign = NotEmptyArray.last signedValue.signatures |> ByteArraySignature.toByteArray  
        //sign bytearray with 
        let signature = Signature.Sign.byteArray256 keyPair btsToSign
        //add signature to list of signatures
        { signedValue with signatures = NotEmptyArray.add signature signedValue.signatures } 

    type ValidateResult<'T> =
        | PartlyValid of Signed<'T>
        | Invalid
        | Valid

        
    //validate signatures for value: Signed<'T> with 
    //list of public keys that should match list of signature
    let validate (map: 'T -> byte[]) (signed:Signed<'T>) (publicCpss:PublicCsp list) =
        let swbas = SignatureWithByteArray.getListFromSigned map signed
        if swbas.Length <> publicCpss.Length then
            false
        else 
            let rec validate' swabsWithKeys =
                match swabsWithKeys with
                | head :: tail  ->
                    let (swab,publicCsp) = head
                    if SignatureWithByteArray.verify swab publicCsp then
                        validate' tail
                    else
                        false
                | [] -> true
            validate' (List.zip swbas publicCpss)






