#load "Fsharp.fs"
#load "String.fs"
#load "ByteArray.fs"
#load "Hash.fs"
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

let keyPwd,ivPwd = Password.getAesKeyFromPassword KeySize.key256  salt  pwd

let fileSource = @"c:\temp\framochtillbaka.txt"
let fileTarget = @"c:\temp\framochtillbaka_test.aes"

let encryptStream source target  =
    use fsSource = File.OpenRead(source)
    let enSource = DecryptedStream fsSource
    use fsTarget = File.OpenWrite (target)
    let enTarget = EncryptTargetStream fsTarget
    Aes.encryptStream  keyPwd ivPwd enSource enTarget
    fsSource.Close()
    fsTarget.Close()


let decryptStream source target = 
    use fsSource = File.OpenRead(source)
    let enSource = EncryptedStream fsSource
    use fsTarget = File.OpenWrite (target)
    let enTarget = DecryptTargetStream fsTarget
    Aes.decryptStream  keyPwd ivPwd enSource enTarget
    fsSource.Close()
    fsTarget.Close()

encryptStream @"c:\temp\framochtillbaka.txt" @"c:\temp\framochtillbaka_test.aes"
decryptStream @"c:\temp\framochtillbaka_test.aes" @"c:\temp\framochtillbaka_decrypted.txt"

//msDecrypted.CopyTo(fsDecrypted)
//fsDecrypted.Close()

//let msDecrypted = Aes.decryptStream key iv msEncrypted
