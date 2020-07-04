#load "Fsharp.fs"
#load "String.fs"
#load "ByteArray.fs"
#load "SymmetricEncryption.fs"

open EncryptCore.SymmetricEncryption
open EncryptCore
open System.IO

let key,iv = Aes.newKeys()
let encryptor' = Aes.encryptByteArray key iv
let decryptor' = Aes.decryptByteArray key iv 

let bts = System.Text.Encoding.UTF8.GetBytes("skdnöalskndöalskndölkasndlökadnö")
bts.Length 
let entryptor =  BytesForSymmetricEncryption.create >> encryptor'
let seb = entryptor bts
let decBts = decryptor' seb

let salt = Salt.create 128
let pwd = Password.create "FarfarsKalsonger@666"

let pwdEncryptor salt pwd = 
    let key,iv = Password.getAesKeyFromPassword KeySize.key256  salt  pwd
    String.toByteArray
    >> BytesForSymmetricEncryption.create 
    >> Aes.encryptByteArray key iv

let pwdDecryptor salt pwd bts = 
    let key,iv = Password.getAesKeyFromPassword KeySize.key256  salt  pwd
    Aes.decryptByteArray key iv bts

let testSentence = "Ett test av kyptering med password"

let pwdEncryptedString = testSentence |> pwdEncryptor salt pwd

let pwdDecryptedString = 
    pwdEncryptedString 
    |> pwdDecryptor salt pwd
    |> SymmecricDecryptedBytes.toByteArray
    |> ByteArray.toString

printfn "Resultatet är %b" (pwdDecryptedString = testSentence)

let fs = File.OpenRead(@"c:\temp\framochtillbaka.txt")
let msEncrypted = Aes.encryptStream key iv fs
let msDecrypted = Aes.decryptStream key iv msEncrypted

let fsDecrypted = File.OpenWrite(@"c:\temp\framochtillbaka_decrypted.txt")
msDecrypted.CopyTo(fsDecrypted)
fs.Close()