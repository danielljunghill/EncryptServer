namespace EncryptCore.Hash
open System.Security.Cryptography
open System.IO

type ShaHash = 
    private
    | Sha256 of byte[]
    | Sha512 of byte[]



module ShaHash =
    let create256 = ShaHash.Sha256
    let create512 = ShaHash.Sha512
    let toByteArray (hash: ShaHash) = 
        match hash with
        | Sha256 bts -> bts
        | Sha512 bts -> bts

module SHA256 =
    let private create() = SHA256.Create()
    module ByteArray =
        let compute (bts: byte[]) =
            use sha256 = create()
            sha256.ComputeHash(bts) |> Sha256
    module File =
        let compute (sr: Stream) =
            use sha256 = create()
            sha256.ComputeHash(sr) |> Sha256

module Sha512 =
    let private create() = SHA512.Create()
    module ByteArray =
        let compute (bts: byte[]) =
            use sha512 = create()
            sha512.ComputeHash(bts) |> Sha512
    module File =
        let compute (sr: Stream) =
            use sha512 = create()
            sha512.ComputeHash(sr) |> Sha512


    
