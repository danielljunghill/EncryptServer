#load "Fsharp.fs"
#load "ByteArray.fs"
#load "String.fs"
#load "AssymetricEncryption.fs"

open EncryptCore.AssymetricEncryption

let pub,priv = RSACryptoServiceProvider.createRsaKeyPair()


let str = "Detta är ett test" 
let btsString = System.Text.Encoding.UTF8.GetBytes(str);

let encryptor = PublicCsp.encrypt pub
let decryptor = KeyPairCsp.decrypt priv

let aev = encryptor btsString
let decryptedBts = decryptor aev |> AssymetricDecryptedBytes.toByteArray
let decryptedStr = System.Text.Encoding.UTF8.GetString(decryptedBts)

let stringToSign = StringToSing.create "Test av signering @ 1223"

EncryptCore.AssymetricEncryption.Sign.string stringToSign priv


