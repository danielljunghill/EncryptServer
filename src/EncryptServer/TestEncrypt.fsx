
#load "Id.fs"
#load "Identity.fs"
#load "AssymetricEncryption.fs"

open System
open System.Security.Cryptography
open System.Xml.Serialization
open System.IO



 open EncryptServer.AssymetricEncryption
 open EncryptServer

//TEST *****************
let pub,priv = RSACryptoServiceProvider.createRsaKeyPair()


let str = "Detta är ett test" 
let btsString = System.Text.Encoding.UTF8.GetBytes(str);

let encryptor = PublicCsp.encrypt pub
let decryptor = KeyPairCsp.decrypt priv

let aev = encryptor btsString
let (ADV decryptedBts) = decryptor aev
let decryptedStr = System.Text.Encoding.UTF8.GetString(decryptedBts)
//******************* TEST



let newId = Id.create()
let idTob64 = Id.toBase64String newId
printfn "%i" idTob64.Length
let idFromb64 = Id.fromBase64String idTob64
//login

//skapa publik och private nyckel med id kopplad till detta och till login alt annat id
//användare behöver nyckel och id -> base64 sträng

    



