#load "Fsharp.fs"
#load "ByteArray.fs"
#load "String.fs"
#load "Hash.fs"
#load "AssymetricEncryption.fs"

open EncryptCore.AssymetricEncryption
open EncryptCore
open Fsharp
let pub,priv = RSACryptoServiceProvider.createRsaKeyPair()


let str = "Detta är ett test" 
let btsString = System.Text.Encoding.UTF8.GetBytes(str);

let encryptor = PublicCsp.encrypt pub
let decryptor = KeyPairCsp.decrypt priv

let aev = encryptor btsString
let decryptedBts = decryptor aev |> AssymetricDecryptedBytes.toByteArray
let decryptedStr = System.Text.Encoding.UTF8.GetString(decryptedBts)

let stringToSign = String.toByteArray 
let stringToSign1 = stringToSign "Ett test för alla oss galningar 1"
let stringToSign2 = stringToSign "Ett test för alla oss galningar 2"

let signedbts = Signature.Sign.byteArray512 priv stringToSign1
let verifiedResult = Signature.Verify.byteArray pub signedbts stringToSign1
let verifiedResult2 = Signature.Verify.byteArray pub signedbts stringToSign2

