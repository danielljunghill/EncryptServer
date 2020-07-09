namespace EncryptCore.Model.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open EncryptCore.Model

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        let encryptKeyPair = EncryptionKeyPair.create()
        let encryptKeyPairString = EncryptionKeyPair.toEncryptionKeyPairString encryptKeyPair
        let s = printf "%A" encryptKeyPairString
        Console.Write(s)
        Assert.IsTrue(true);
