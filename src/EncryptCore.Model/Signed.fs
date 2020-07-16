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
                    
    let createAndSign map value keyPair  =
        {
            value = value 
            signatures =   Signature.Sign.byteArray256 keyPair (map value) |> NotEmptyArray.create
        }

    let sign (signedValue:Signed<'T>) keyPair =
        let btsToSign = NotEmptyArray.last signedValue.signatures |> ByteArraySignature.toByteArray  
        let signature = Signature.Sign.byteArray256 keyPair btsToSign
        { signedValue with signatures = NotEmptyArray.add signature signedValue.signatures }  

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

        //let verifyLength (signatures: ByteArraySignature[]) (publicCCpsArr: PublicCsp[]) = (signatures.Length + 1) <> publicCpss.Length
        //let verifySignature  (originalValue: byte[]) (signatures: ByteArraySignature[]) (publicCCpsArr: PublicCsp[]) level =
        //        let signedBytes =
        //            if level = 0 then

             
        //if (signatures.Length + 1) <> publicCpss.Length then
        //     false
        //else    
        //    let rec validate' level  =
        //       if level = 0 theh
        //            true
        //       else
                  

   
        //let validate map (signedValue: Signed<'T>)  =
    //    fun publicScp -> Signature.Verify.byteArray publicScp signedValue.signature (map signedValue.value)
    
    //let sign 