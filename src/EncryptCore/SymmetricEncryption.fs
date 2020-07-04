namespace EncryptCore.SymmetricEncryption
open System
open System.IO
open System.Security.Cryptography
open EncryptCore.Fsharp

type BytesForSymmetricEncryption = private | BytesForSymmetricEncryption of byte[]
type SymmetricEncryptedBytes = private | SymmetricEncryptedBytes of byte[]
type SymmecricDecryptedBytes = private | SymmecricDecryptedBytes of byte[]

module BytesForSymmetricEncryption =
    let create  =
        fun (bts:Byte[]) -> [ BitConverter.GetBytes(bts.Length) ; bts ] 
        >> Array.concat
        >> BytesForSymmetricEncryption

module SymmecricDecryptedBytes =
    let private getLength bts =
        BitConverter.ToInt32(bts,0), bts
    let private getByteArray' (len,(bts:byte[]))  =
        Array.sub bts 4 len
    let create  = 
        getLength
        >> getByteArray'
        >> SymmecricDecryptedBytes
    let toByteArray (SymmecricDecryptedBytes bts) = bts

type Key = private | Key of byte[]
module Key =
    let toByteArray (Key bts) = bts

type IV = private | IV of byte[]
module IV =
    let toByteArray (IV bts) = bts

type Encryptor = private | Encryptor of ICryptoTransform
module Encryptor =
    let create (Key key) (IV iv) (aes: Aes) =
        aes.CreateEncryptor(key,iv) |> Encryptor
    module ByteArray = 
        let encrypt (BytesForSymmetricEncryption bts) (Encryptor ict)   =
            use ms = new MemoryStream()
            use cs = new CryptoStream(ms,ict,CryptoStreamMode.Write)
            cs.Write(bts,0,bts.Length)
            cs.FlushFinalBlock()
            ms.ToArray() 

    module Stream =
        let encrypt (sr: Stream) (Encryptor ict) =
            use ms = new MemoryStream()
            use cs = new CryptoStream(ms,ict,CryptoStreamMode.Write) 
            sr.CopyTo (cs)
            cs.FlushFinalBlock()
            new MemoryStream(ms.ToArray())  :> Stream
    
type Decryptor = private | Decryptor of ICryptoTransform
module Decryptor=
    let create (Key key) (IV iv) (aes: Aes) =
        aes.CreateDecryptor(key,iv)  |> Decryptor
    module ByteArray = 
        let decrypt (SymmetricEncryptedBytes bts) (Decryptor ict) = 
            use ms = new MemoryStream(bts)
            use cs = new CryptoStream(ms,ict,CryptoStreamMode.Read)
            let decrypted = Array.zeroCreate<byte> bts.Length
            let bytesRead = cs.Read(decrypted,0,bts.Length)
            decrypted 
            |> Array.take bytesRead
            |> SymmecricDecryptedBytes.create

     module Stream =
        let decrypt (sr: Stream) (Decryptor ict) =
            sr.Position <- 0L
            use ms = new MemoryStream()
            use cs = new CryptoStream(sr,ict,CryptoStreamMode.Read)
            cs.CopyTo(ms)
            new MemoryStream(ms.ToArray()) :> Stream
            //let decrypted = Array.zeroCreate<byte> bts.Length
            //let bytesRead = cs.Read(decrypted,0,bts.Length)
            //decrypted 
            //|> Array.take bytesRead
            //|> SymmecricDecryptedBytes.create


module Aes =
    let newAes() =
        let aes = Aes.Create()
        aes.KeySize <- 256
        aes.BlockSize <- 128
        aes.FeedbackSize <- 128
        aes.Padding <- PaddingMode.Zeros  
        aes

    let newKeys =
        newAes
        >> fun (aes: Aes) -> Key aes.Key, IV aes.IV

    let encryptByteArray key iv bfs =
        newAes
        >> Encryptor.create key iv
        >> Encryptor.ByteArray.encrypt bfs
        >> SymmetricEncryptedBytes
        |> ce

    let decryptByteArray key iv bfs =
        newAes
        >> Decryptor.create key iv
        >> Decryptor.ByteArray.decrypt bfs
        |> ce 


    let encryptStream key iv sr =
        newAes
        >> Encryptor.create key iv
        >> Encryptor.Stream.encrypt sr
        |> ce

    let decryptStream key iv bfs =
        newAes
        >> Decryptor.create key iv
        >> Decryptor.Stream.decrypt bfs
        |> ce 

type Salt = private | Salt of byte[]
module Salt =
    let toByteArray (Salt bts) = bts
    let create length =
        let bts = Array.zeroCreate<byte> length
        let rand = new RNGCryptoServiceProvider()
        rand.GetBytes(bts)
        Salt bts

type KeySize = private | KeySize of int
module KeySize =
    let toInt (KeySize size) = size
    let key128 = KeySize 128
    let key256 = KeySize 256

module SymmetricAesAlgorithm =
    let create size =
        SymmetricAlgorithm.Create "AES"
        |> fun alg -> 
            alg.KeySize <- KeySize.toInt size
            alg
       
type Password = private | Password of string
module Password =
   
    let create = Password
    let getPasswordDeriveBytes salt (Password pwd) =
        let pdb = new PasswordDeriveBytes(pwd,salt,"SHA512",10)
        pdb
    //let getAesFromPassword salt pwd  =
    //    let aes = createAes()
    //    let pdb = getPasswordDeriveBytes salt pwd
    //    aes.Key <- pdb.CryptDeriveKey("AES","SHA1",256,aes.IV)

    let private getKeyFromPassword' (algorithm: SymmetricAlgorithm) (salt: Salt) (Password pwd) =
         let rfc2898DeriveBytes = new Rfc2898DeriveBytes(pwd, Salt.toByteArray salt);
         let key = rfc2898DeriveBytes.GetBytes(algorithm.KeySize / 8);
         let iv =  rfc2898DeriveBytes.GetBytes(algorithm.BlockSize / 8); 
         Key key,IV iv  

    let getAesKeyFromPassword  = SymmetricAesAlgorithm.create >> getKeyFromPassword' 




    



