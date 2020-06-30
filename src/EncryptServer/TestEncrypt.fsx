﻿
#load "Id.fs"
//#load "Identity.fs"
//#load "AssymetricEncryption.fs"





// open EncryptServer.AssymetricEncryption
// open EncryptServer

////TEST *****************
//let pub,priv = RSACryptoServiceProvider.createRsaKeyPair()


//let str = "Detta är ett test" 
//let btsString = System.Text.Encoding.UTF8.GetBytes(str);

//let encryptor = PublicCsp.encrypt pub
//let decryptor = KeyPairCsp.decrypt priv

//let aev = encryptor btsString
//let (ADV decryptedBts) = decryptor aev
//let decryptedStr = System.Text.Encoding.UTF8.GetString(decryptedBts)
////******************* TEST



//let newId = Id.create()
//let idTob64 = Id.toBase64String newId
//printfn "%i" idTob64.Length
//let idFromb64 = Id.fromBase64String idTob64
//login

//skapa publik och private nyckel med id kopplad till detta och till login alt annat id
//användare behöver nyckel och id -> base64 sträng
open System
open System.Security.Cryptography
open System.Xml.Serialization
open System.IO
open System.IO
let aes1 = Aes.Create()

let key = aes1.Key
let iv = aes1.IV

let createAes key iv =
    let aes = Aes.Create()
    aes.KeySize <- 256
    aes.BlockSize <- 128
    aes.FeedbackSize <- 128
    aes.Padding <- PaddingMode.Zeros  
    aes.IV <- iv
    aes.Key <- key
    aes

let decryptEncrypt ict bts =
    use ms = new MemoryStream()
    use cs = new CryptoStream(ms,ict,CryptoStreamMode.Write)
    cs.Write(bts,0,bts.Length)
    cs.FlushFinalBlock()
    ms.ToArray()


let encrypt key iv (bts:byte[]) =
    let aes = createAes key iv  
    let ict = aes.CreateEncryptor(key,iv) 
    decryptEncrypt ict bts

let decrypt key iv (encBts:byte[]) =
    let aes = createAes key iv 
 
    let ict = aes.CreateDecryptor(key,iv)
    decryptEncrypt ict encBts
  
let encryptor = encrypt key iv
let decryptor = decrypt key iv 

let bts = System.Text.Encoding.UTF8.GetBytes("skdnöalskndöalskndölkasndlökadnö")
bts.Length 
let encBts = encryptor bts
encBts.Length
let decBts = decryptor encBts 





    



