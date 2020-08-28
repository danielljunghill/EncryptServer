namespace EncryptCore.Model.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open EncryptCore.Model
open EncryptCore.AssymetricEncryption
open EncryptCore

[<TestClass>]
type SignTest () =

    [<TestMethod>]
    member this.SignTest() =
        let privateKey = PrivateKey.create()
        let signedString = Signed.createAndSign privateKey String.toByteArray "Kakadua" 
        let publicKey = PrivateKey.toPublicKey privateKey
        let result = Signed.validate String.toByteArray signedString [publicKey]
        Assert.IsTrue(result)

    [<TestMethod>]
    member this.ReSignTest() =
        let firstPrivateKey = PrivateKey.create()
        let secondPrivateKey = PrivateKey.create()
        let signedValue = 
            Signed.createAndSign firstPrivateKey String.toByteArray 
            >> Signed.sign secondPrivateKey
          
        let firstPublicKey = PrivateKey.toPublicKey firstPrivateKey
        let secondPublicKey = PrivateKey.toPublicKey secondPrivateKey
        let result = Signed.validate String.toByteArray (signedValue "Kakadua") [firstPublicKey ;secondPublicKey   ]
        Assert.IsTrue(result)



    [<TestMethod>]
    member this.ReReSignTest() =
        let firstPrivateKey = PrivateKey.create()
        let secondPrivateKey = PrivateKey.create()
        let thirdPrivateKey = PrivateKey.create()
        let signedValue =
            Signed.createAndSign firstPrivateKey String.toByteArray "Kakadua" 
            |> Signed.sign secondPrivateKey
            |> Signed.sign thirdPrivateKey

        let firstPublicKey = PrivateKey.toPublicKey firstPrivateKey
        let secondPublicKey = PrivateKey.toPublicKey secondPrivateKey
        let thirdPublicKey = PrivateKey.toPublicKey thirdPrivateKey
        let result = Signed.validate String.toByteArray signedValue [firstPublicKey ;secondPublicKey; thirdPublicKey   ]
        Assert.IsTrue(result)


    [<TestMethod>]
    member this.SignedPrivateIdKey () =
        let signedPrivateIdKey = SignedPrivateIdKey.create()
        let result = SignedPrivateIdKey.isValid signedPrivateIdKey
        Assert.IsTrue result

