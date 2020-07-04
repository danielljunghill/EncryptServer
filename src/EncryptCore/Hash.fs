namespace EncryptCore.Hash
open System.Security.Cryptography
open System.IO

type Sha256HashedValue = private | Sha256HashedValue of byte[]
module Sha256HashedValue =
    let create = Sha256HashedValue
    let toByteArray (Sha256HashedValue bts) = bts

module SHA256 =
    let private create() = SHA256.Create()
    module ByteArray =
        let compute (bts: byte[]) =
            use sha256 = create()
            sha256.ComputeHash(bts) |> Sha256HashedValue
    module File =
        let compute (sr: Stream) =
            use sha256 = create()
            sha256.ComputeHash(sr) |> Sha256HashedValue

    
