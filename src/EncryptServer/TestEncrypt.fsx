
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
open System
let strToBytes (s: string)=
    System.Text.Encoding.UTF8.GetBytes(s)

let bytesToString (bts: byte[]) =
    System.Text.Encoding.UTF8.GetString(bts)

type BytesForSymmetricEncryption = private | BytesForSymmetricEncryption of byte[]
type SymmetricEncryptedBytes = private | SymmetricEncryptedBytes of byte[]
type SymmecricDecryptedBytes = private | SymmecricDecryptedBytes of byte[]

module BytesForSymmetricEncryption =
    let create  =
        fun (bts:Byte[]) -> [ BitConverter.GetBytes(bts.Length) ; bts ] 
        >> Array.concat
        >> BytesForSymmetricEncryption

        

module SymmecricDecryptedBytes =
    let private getLength bts =
        BitConverter.ToInt32(bts,0), bts
    let private getByteArray' (len,(bts:byte[]) )  =
        Array.sub bts 4 len
    let create  = 
        getLength
        >> getByteArray'
        >> SymmecricDecryptedBytes

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


let cr f = f()

type Key = private | Key of byte[]
module Key =
    let toByteArray (Key bts) = bts

type IV = private | IV of byte[]
module IV =
    let toByteArray (IV bts) = bts

type Encryptor = private | Encryptor of ICryptoTransform
module Encryptor =
    let create (Key key) (IV iv) (aes: Aes) =
        aes.CreateEncryptor(key,iv) |> Encryptor

    let encrypt (BytesForSymmetricEncryption bts) (Encryptor ict)   =
        use ms = new MemoryStream()
        use cs = new CryptoStream(ms,ict,CryptoStreamMode.Write)
        cs.Write(bts,0,bts.Length)
        cs.FlushFinalBlock()
        ms.ToArray() 
    
type Decryptor = private | Decryptor of ICryptoTransform
module Decryptor=
    let create (Key key) (IV iv) (aes: Aes) =
        aes.CreateDecryptor(key,iv)  |> Decryptor

    let decrypt (SymmetricEncryptedBytes bts) (Decryptor ict) = 
        use ms = new MemoryStream(bts)
        use cs = new CryptoStream(ms,ict,CryptoStreamMode.Read)
        let decrypted = Array.zeroCreate<byte> bts.Length
        let bytesRead = cs.Read(decrypted,0,bts.Length)
        decrypted 
        |> Array.take bytesRead
        |> SymmecricDecryptedBytes.create

module Aes =
    let newAes() =
        let aes = Aes.Create()
        aes.KeySize <- 256
        aes.BlockSize <- 128
        aes.FeedbackSize <- 128
        aes.Padding <- PaddingMode.Zeros  
        aes

    let newKeys =
        newAes
        >> fun (aes: Aes) -> Key aes.Key, IV aes.IV

    let encrypt key iv bfs =
        newAes
        >> Encryptor.create key iv
        >> Encryptor.encrypt bfs
        >> SymmetricEncryptedBytes
        |> cr

    let decrypt key iv bfs =
        newAes
        >> Decryptor.create key iv
        >> Decryptor.decrypt bfs
        |> cr

let key,iv = Aes.newKeys()
let encryptor' = Aes.encrypt key iv
let decryptor' = Aes.decrypt key iv 

let bts = System.Text.Encoding.UTF8.GetBytes("skdnöalskndöalskndölkasndlökadnö")
bts.Length 
let entryptor =  BytesForSymmetricEncryption.create >> encryptor'
let seb = entryptor bts
let decBts = decryptor' seb


module Password =
    let createSalt length =
        let bts = Array.zeroCreate<byte> length
        let rand = new RNGCryptoServiceProvider()
        rand.GetBytes(bts)
        bts

    let getPasswordDeriveBytes salt (pwd: string) =
        let pwdBts = System.Text.Encoding.Unicode.GetBytes(pwd) 
        let tdes = new TripleDESCryptoServiceProvider()
        let pdb = new PasswordDeriveBytes(pwd,salt,"SHA512",10)
        pdb
    //let getAesFromPassword salt pwd  =
    //    let aes = createAes()
    //    let pdb = getPasswordDeriveBytes salt pwd
    //    aes.Key <- pdb.CryptDeriveKey("AES","SHA1",256,aes.IV)

    let getKeyAndIVFromPassword (algorithm: SymmetricAlgorithm) (salt: byte[]) pwd =
         let rfc2898DeriveBytes = new Rfc2898DeriveBytes(pwd, salt);
         let key = rfc2898DeriveBytes.GetBytes(algorithm.KeySize / 8);
         let iv =  rfc2898DeriveBytes.GetBytes(algorithm.BlockSize / 8); 
         key,iv  

let salt = Password.createSalt 128
let alg = (SymmetricAlgorithm.Create("AES"))
alg.KeySize <-256

let pwdkey,pwdIv = Password.getKeyAndIVFromPassword alg  salt "Kakadua12"
let encryptorPwd = encrypt pwdkey pwdIv

let testBts = strToBytes "Testing decryption"
let testBtsEncrypted = encryptorPwd testBts
let pwdkey2,pwdIv2 = Password.getKeyAndIVFromPassword alg  salt "Kakadua12"
let decryptorPwd = decrypt pwdkey2 pwdIv2
let testBtsDecrypted = decryptorPwd testBtsEncrypted
let testStringDecrypted = bytesToString testBtsDecrypted
    



