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

            printfn "verify %A" swba.signedByteArray
            printfn "signature %A" swba.signature
            Signature.Verify.byteArray publicCps swba.signature swba.signedByteArray

        let getListFromSigned map (signedValue: Signed<_>) =
            let rec getListFromSignatures signedByteArrays =
                match signedByteArrays with
                | head :: [] ->        
                    [ { signedByteArray = signedValue.value |> map ; signature = head } ]
                | head :: tail ->         
                    { signedByteArray = ByteArraySignature.toByteArray tail.Head; signature = head } :: getListFromSignatures tail
                | [] -> []
            getListFromSignatures (NotEmptyArray.toList signedValue.signatures |> List.rev)

    let sign privateKey signed  =
        //take bytearray for last signature
        let btsToSign = NotEmptyArray.last signed.signatures |> ByteArraySignature.toByteArray  
        printfn "signing %A" btsToSign
        //sign bytearray with 
        let signature = Signature.Sign.byteArray256 privateKey btsToSign
        printfn "signature %A" signature
        //add signature to list of signatures
        { signed with signatures = NotEmptyArray.add signature signed.signatures } 

    let rec signMany privateKeys signed  =
        match privateKeys with
        | key :: [] -> sign key signed 
        | key :: tail -> 
            let newSigned = sign key signed 
            signMany tail newSigned 
    
        | [] ->  System.Exception("No keys provided when signing many") |> raise
        

     //create Signed and add first signature of original value            
    let createAndSign privateKey map value   =
        let btsToSign = (map value) 
        printfn "signing %A" btsToSign
        let signature = Signature.Sign.byteArray256 privateKey btsToSign  |> NotEmptyArray.create
        printfn "signature %A" signature

        {
            value = value 
            signatures =   signature
        }



        
     

    type ValidateResult<'T> =
        | PartlyValid of Signed<'T>
        | Invalid
        | Valid


    //validate signatures for value: Signed<'T> with 
    //list of public keys that should match list of signature
    let validate map signed (publicKeys:PublicKey list) =
        let swbas = SignatureWithByteArray.getListFromSigned map signed
        if swbas.Length <> publicKeys.Length then
            false
        else 
            let rec validate' swabsWithKeys =
                match swabsWithKeys with
                | head :: tail  ->
                    let (swab,publicKey) = head
                    if SignatureWithByteArray.verify swab publicKey then
                        validate' tail
                    else
                        false
                | [] -> true
            validate' (List.zip swbas (publicKeys |> List.rev) )






