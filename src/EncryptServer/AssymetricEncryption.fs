namespace EncryptServer.AssymetricEncryption

open System.Security.Cryptography
open System
open Newtonsoft.Json.Linq

type PublicCsp = | PublicCsp of byte[]


type KeyPairCsp = 
    | KeyPairCsp of byte[]

//Assymetric encrypted bytes
type AEV  = | AEV of byte[]
//Assymetric decrypted bytes
type ADV = | ADV of byte[]

module AEV =
    let toB64String  =
        fun (AEV bts) -> Convert.ToBase64String bts
    let fromB64String =
        Convert.FromBase64String
        >> AEV



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
         >> fun provider -> fun bts -> provider.Encrypt(bts,false) |> AEV
     module Json =
        let private name = "PublicCsp"
        let asJProperty pc =  JProperty(name, toB64String pc)
        let fromJObject  =
            fun (kr: JObject) -> kr.[name] 
            >> string
            >> fromB64String

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
        >> (fun provider -> (fun (AEV bts) -> provider.Decrypt(bts,false) |> ADV))
    module Json =
        let private name = "KeyPairCsp"
        let asJProperty kp  =  JProperty(name, toB64String kp)
        let fromJObject  =
            fun (kr: JObject) -> kr.[name] 
            >> string
            >> fromB64String


