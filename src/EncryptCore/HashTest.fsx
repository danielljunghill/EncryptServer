#load "Hash.fs"
#load "ByteArray.fs"
#load "String.fs"
open EncryptCore.Hash
open EncryptCore
open System.IO

let hashString =
    String.toByteArray >> SHA256.ByteArray.compute >> Sha256HashedValue.toByteArray >> ByteArray.toBase64String
let hash = hashString "asjdbklajbdkabsdlkjasd"
let hashFile =
    SHA256.File.compute >> Sha256HashedValue.toByteArray >> ByteArray.toBase64String
let fs =  File.OpenRead(@"c:\temp\EBA_2811_Testdata.zip")
try
    let hs = hashFile fs
    printfn "%s" hs
finally
    fs.Close()

