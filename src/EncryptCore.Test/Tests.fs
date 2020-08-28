namespace EncryptCore.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open EncryptCore.Model

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        //let clientIdentity = ClientIdentity.create
        //printfn "%A" clientIdentity
        Assert.IsTrue(true);
