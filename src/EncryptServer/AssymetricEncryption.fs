namespace EncryptServer.AssymetricEncryption

open System.Security.Cryptography
open System

type PublicCsp = | PublicCsp of byte[]
type PublicCspString = | PublicCspString of string

type KeyPairCsp = 
    | KeyPairCsp of byte[]

type KeyPairCspString =
    | KeyPairCspString of string

type AEV  = | AEV of byte[]
type ADV = | ADV of byte[]

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
     let asB64String  =
         fun (PublicCsp blob) -> Convert.ToBase64String blob
         >> PublicCspString
     let fromB64String =
          fun (PublicCspString str) -> Convert.FromBase64String  str
          >> PublicCsp
     let asProvider  =
          fun (PublicCsp blob) -> RSACryptoServiceProvider.importCsaBlob blob
     let encrypt  =
         asProvider 
         >> fun provider -> fun bts -> provider.Encrypt(bts,false) |> AEV

module KeyPairCsp =
    let asB64String =
        fun (KeyPairCsp blob) -> Convert.ToBase64String(blob)   
        >> KeyPairCspString
    let fromB64String =
        (fun (KeyPairCspString str) -> Convert.FromBase64String  str) 
        >> KeyPairCsp
    let asPublicCsp  =
         (fun (KeyPairCsp blob) -> RSACryptoServiceProvider.importCsaBlob blob)  
         >> (fun provider -> provider.ExportCspBlob(false)) 
         >> PublicCsp
    let asProvider  =
         fun (KeyPairCsp blob) ->  RSACryptoServiceProvider.importCsaBlob blob
    let decrypt  =
        asProvider
        >> (fun provider -> (fun (AEV bts) -> provider.Decrypt(bts,false) |> ADV))
    