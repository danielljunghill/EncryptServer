namespace EncryptCore.Model.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open EncryptCore.Model

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.PrivateAccountIsValidTest () =
        let privateAccount = PrivateAccount.create()
        let result = PrivateAccount.isValid privateAccount
        Assert.IsTrue result


    [<TestMethod>]
    member this.PublicAccountIsNotValidTest () =
        let privateAccount = PrivateAccount.create()
        let publicAccount = PrivateAccount.toPublicAccount privateAccount
        let result = PrivateAccount.create() |> PublicAccount.isValid publicAccount 
        Assert.IsFalse result


    [<TestMethod>]
    member this.PublicAccountIsValidTest () =
        let privateAccount = PrivateAccount.create()
        let publicAccount = PrivateAccount.toPublicAccount privateAccount
        let result = PublicAccount.isValid publicAccount privateAccount
        Assert.IsTrue result


    [<TestMethod>]
    member this.SignedPrivateIdKey () =
        let signedPrivateIdKey = SignedPrivateIdKey.create()
        let result = SignedPrivateIdKey.isValid signedPrivateIdKey 
        Assert.IsTrue result