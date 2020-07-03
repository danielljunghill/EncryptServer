namespace EncryptCore.AssymetricEncryption
open EncryptCore
open System.Security.Cryptography
open EncryptCore.Fsharp
open System

type PublicCsp =  private| PublicCsp of byte[]

type  KeyPairCsp = private | KeyPairCsp of byte[]

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

type ExportKeyParameter = 
    | IncludePublicKeyOnly 
    | IncludeBoth 

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
    let exportParameter param (rsAlg: RSACryptoServiceProvider)  =
        match param with
        | IncludePublicKeyOnly ->
            rsAlg.ExportParameters(false)
        | IncludeBoth ->
            rsAlg.ExportParameters(true)

module PublicCsp =
     let toB64String  =
         fun (PublicCsp blob) -> Convert.ToBase64String blob
     let fromB64String =
          Convert.FromBase64String 
          >> PublicCsp 
     let toRsaAlg  =
          fun (PublicCsp blob) -> RSACryptoServiceProvider.importCsaBlob blob
     let encrypt  =
         toRsaAlg 
         >> fun rsaAlg -> fun bts -> rsaAlg.Encrypt(bts,false) |> AssymetricEncryptedBytes

module KeyPairCsp =
    let toB64String =
        fun (KeyPairCsp blob) -> Convert.ToBase64String(blob)   
    let fromB64String =
        Convert.FromBase64String
        >> KeyPairCsp
    let toPublicCsp  =
         (fun (KeyPairCsp blob) -> RSACryptoServiceProvider.importCsaBlob blob)  
         >> (fun rsaAlg -> rsaAlg.ExportCspBlob(false)) 
         >> PublicCsp
    let toRsaAlg  =
         fun (KeyPairCsp blob) ->  RSACryptoServiceProvider.importCsaBlob blob
    let decrypt  =
        toRsaAlg
        >> (fun rsaAlg -> (fun (AssymetricEncryptedBytes bts) -> rsaAlg.Decrypt(bts,false) |> AssymetricDecryptedBytes))




type BytesToSign = private | BytesToSign of byte[]
module BytesToSign =
    let create = BytesToSign
    let toByteArray (BytesToSign bts) = bts

type SignedData = private | SignedData of byte[]
module SignedData =
    let toByteArray (SignedData bts) =  bts
    
module Signature =
    module Sign =
        let byteArray keyPair bytesToSign  =
            let rsaAlg = KeyPairCsp.toRsaAlg keyPair
            rsaAlg.SignData(BytesToSign.toByteArray bytesToSign, new SHA512CryptoServiceProvider())
            |> SignedData

    module Verify =
        let byteArray  =
            let verifyData (rsaAlg:RSACryptoServiceProvider) signedData (BytesToSign bts) =
                rsaAlg.VerifyData(bts, new SHA512CryptoServiceProvider(), SignedData.toByteArray signedData)
            PublicCsp.toRsaAlg 
            >> verifyData  

