#load "SymmetricEncryption.fs"
open EncryptCore.SymmetricEncryption
open EncryptCore
let key,iv = Aes.newKeys()
let encryptor' = Aes.encrypt key iv
let decryptor' = Aes.decrypt key iv 

let bts = System.Text.Encoding.UTF8.GetBytes("skdnöalskndöalskndölkasndlökadnö")
bts.Length 
let entryptor =  BytesForSymmetricEncryption.create >> encryptor'
let seb = entryptor bts
let decBts = decryptor' seb

let salt = Salt.create 128
let pwd = Password.create "FarfarsKalsonger@666"

let pwdEncryptor salt pwd = 
    let key,iv = Password.getAesKeyFromPassword KeySize.key256  salt  pwd
    Enc
    >> BytesForSymmetricEncryption.create 
    >> Aes.encrypt key iv
let pwdDecryptor salt pwd bts = 
    let key,iv = Password.getAesKeyFromPassword KeySize.key256  salt  pwd
    Aes.decrypt key iv bts



let pwdEncryptedString = "Ett test av kyptering med password" |> pwdEncryptor salt pwd

let pwdDecryptedString = 
    pwdEncryptedString 
    |> pwdDecryptor salt pwd
    |> SymmecricDecryptedBytes.toByteArray
    |> bytesToString
