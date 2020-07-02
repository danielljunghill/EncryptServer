namespace EncryptCore.AssymetricEncryption
open System.Security.Cryptography
open System

type PublicCsp = | PublicCsp of byte[]

type KeyPairCsp = 
    | KeyPairCsp of byte[]

type AssymetricDecryptedBytes = private | AssymetricDecryptedBytes of byte[]
module AssymetricDecryptedBytes =
    let toByteArray (AssymetricDecryptedBytes bts) = bts

type AssymetricEncryptedBytes  = private | AssymetricEncryptedBytes of byte[]
module AssymetricEncryptedBytes =
    let toB64String  =
        fun (AssymetricEncryptedBytes bts) -> Convert.ToBase64String bts
    let fromB64String =
        Convert.FromBase64String
        >> AssymetricEncryptedBytes
    let toByteArray (AssymetricEncryptedBytes bts) = bts

module RSACryptoServiceProvider =
    let create() =
        new RSACryptoServiceProvider()
    let createRsaKeyPair() =
        let provider = create()
        provider.ExportCspBlob(false) |> PublicCsp, provider.ExportCspBlob(true) |> KeyPairCsp     
    let importCsaBlob keyBlob =
        let provider = create()
        provider.ImportCspBlob(keyBlob)
        provider

module PublicCsp =
     let toB64String  =
         fun (PublicCsp blob) -> Convert.ToBase64String blob
     let fromB64String =
          Convert.FromBase64String 
          >> PublicCsp 
     let toProvider  =
          fun (PublicCsp blob) -> RSACryptoServiceProvider.importCsaBlob blob
     let encrypt  =
         toProvider 
         >> fun provider -> fun bts -> provider.Encrypt(bts,false) |> AssymetricEncryptedBytes

module KeyPairCsp =
    let toB64String =
        fun (KeyPairCsp blob) -> Convert.ToBase64String(blob)   
 
    let fromB64String =
        Convert.FromBase64String
        >> KeyPairCsp
    let toPublicCsp  =
         (fun (KeyPairCsp blob) -> RSACryptoServiceProvider.importCsaBlob blob)  
         >> (fun provider -> provider.ExportCspBlob(false)) 
         >> PublicCsp
    let toProvider  =
         fun (KeyPairCsp blob) ->  RSACryptoServiceProvider.importCsaBlob blob
    let decrypt  =
        toProvider
        >> (fun provider -> (fun (AssymetricEncryptedBytes bts) -> provider.Decrypt(bts,false) |> AssymetricDecryptedBytes))


