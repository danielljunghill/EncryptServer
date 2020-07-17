#r "../EncryptCore/bin/Debug/netcoreapp3.1/EncryptCore.dll"
#load "Identity.fs"
open  System.Security.Cryptography
open EncryptCore.Model
Identity.create



let getListFromSigned transform =
    //let signedByteArrays = NotEmptyArray.toList signedValue.signatures
    let rec getListFromSignatures transformList =
        match transformList with
        | head :: [] -> 
            [ head ]
        | head :: tail ->
      
            head :: getListFromSignatures tail
        | [] -> []
    getListFromSignatures transform


let newList = getListFromSigned [ 1 ; 2 ; 3 ; 4 ; 5]


let testList = 1 :: [ 2 ; 3]

