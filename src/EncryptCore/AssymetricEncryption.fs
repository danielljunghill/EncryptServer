namespace EncryptCore.AssymetricEncryption
open EncryptCore
open System.Security.Cryptography
open EncryptCore.Fsharp
open System

type PublicKey =  private| PublicCsp of byte[]

type  PrivateKey = private | KeyPairCsp of byte[]

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
    
module PublicKey =
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
     let fromKeyPair (KeyPairCsp bts) =
         let provider = RSACryptoServiceProvider.importCsaBlob bts
         provider.ExportCspBlob(false) |> PublicCsp
     let toByteArray (PublicCsp bts) = bts

module PrivateKey =
    let create() = 
        let _, kp = RSACryptoServiceProvider.createRsaKeyPair()
        kp
    let toByteArray (KeyPairCsp blob) = blob
    let toB64String =
        fun (KeyPairCsp blob) -> Convert.ToBase64String(blob)   
    let fromB64String =
        Convert.FromBase64String
        >> KeyPairCsp
    let toPublicKey  =
         (fun (KeyPairCsp blob) -> RSACryptoServiceProvider.importCsaBlob blob)  
         >> (fun rsaAlg -> rsaAlg.ExportCspBlob(false)) 
         >> PublicCsp
    let toRsaAlg  =
         fun (KeyPairCsp blob) ->  RSACryptoServiceProvider.importCsaBlob blob
    let decrypt  =
        toRsaAlg
        >> (fun rsaAlg -> (fun (AssymetricEncryptedBytes bts) -> rsaAlg.Decrypt(bts,false) |> AssymetricDecryptedBytes))

type HashSignature = 
    private 
    |  SHA256HashSignature of byte[]
    |  SHA512HashSignature of byte[]

module HashSignature =
    let toByteArray signature = 
        match signature with
        | SHA256HashSignature bts -> bts
        | SHA512HashSignature bts -> bts


type ByteArraySignature = 
    private 
    |  SHA256ByteArraySignature of byte[]
    |  SHA512ByteArraySignature of byte[]

module ByteArraySignature =
    let toByteArray signature = 
        match signature with
        | SHA256ByteArraySignature bts -> bts
        | SHA512ByteArraySignature bts -> bts  

open EncryptCore.Hash
module Signature =
    module Sign =

        let private byteArray' provider ftype keyPair (bts: byte[])  =
            let rsaAlg = PrivateKey.toRsaAlg keyPair
            rsaAlg.SignData(bts, provider)
            |> ftype
        let byteArray256 = byteArray' (new SHA256CryptoServiceProvider()) SHA256ByteArraySignature
        let byteArray512 = byteArray' (new SHA512CryptoServiceProvider()) SHA512ByteArraySignature
        let private hash' algorithm ftype = 
            fun keypair hash ->
                let rsaAlg = PrivateKey.toRsaAlg keypair
                rsaAlg.SignHash(ShaHash.toByteArray hash, algorithm,RSASignaturePadding.Pss)
                |> ftype
        let hash256 = hash' HashAlgorithmName.SHA256 SHA256HashSignature
        let hash512 = hash' HashAlgorithmName.SHA512 SHA256HashSignature    

    module Verify =
        let byteArray publicCsp (signature: ByteArraySignature)  (bts: byte[]) =
            let rsaAlg = PublicKey.toRsaAlg publicCsp
            match signature with
            | SHA256ByteArraySignature signatureBts ->
                rsaAlg.VerifyData(bts, new SHA256CryptoServiceProvider(), signatureBts)
            | SHA512ByteArraySignature signatureBts ->
                rsaAlg.VerifyData(bts, new SHA512CryptoServiceProvider(), signatureBts)

        let verifyHash publicCsp signature hash =
            let rsaAlg = PublicKey.toRsaAlg publicCsp
            match signature with
            | SHA256HashSignature signatureBts ->
                rsaAlg.VerifyHash(ShaHash.toByteArray hash, signatureBts,HashAlgorithmName.SHA256,RSASignaturePadding.Pss)
            | SHA512HashSignature signatureBts ->
                rsaAlg.VerifyHash(ShaHash.toByteArray hash, signatureBts,HashAlgorithmName.SHA512,RSASignaturePadding.Pss)
        



