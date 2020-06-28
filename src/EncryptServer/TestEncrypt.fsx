open System
open System.Security.Cryptography
open System.Xml.Serialization
open System.IO

    
type PublicCsp = | PublicCsp of byte[]
type PublicCspString = | PublicCspString of string


type KeyPairCsp = 
    | KeyPairCsp of byte[]

type KeyPairCspString =
    | KeyPairCspString of string

type AEV  = | AEV of byte[]

type ADV = | ADV of byte[]


module Base64String =
    let guidToBase64String (gid: Guid) =
        gid.ToByteArray()
        |> Convert.ToBase64String

    let private guidLength' =
        guidToBase64String
        >> fun b64str -> b64str.Length
       
    let private guidLength = Guid.Parse("D89C11A1-44CB-4444-BE6B-7969475212A7") |> guidLength' 

    let getGuidString (str: string) =
        if str.Length < guidLength then
            raise (new ArgumentOutOfRangeException("Id representation to short",str))
        str.Substring(0,guidLength), str.Substring(guidLength, str.Length - guidLength)

//TEST *******************************
let id = Guid.NewGuid() |> Base64String.guidToBase64String
Base64String.getGuidString id

//******************************* TEST

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
    
   
module PublicCsp =

    let prefix = Guid.Parse("E1E357DE-A7A4-4D0C-A1D2-EE2322E07826")

    let asB64String  =
        fun (PublicCsp blob) -> Convert.ToBase64String blob
        >> PublicCspString

    let fromB64String =
         fun (PublicCspString str) -> Convert.FromBase64String  str
         >> PublicCsp

    let asProvider  =
         fun (PublicCsp blob) -> RSACryptoServiceProvider.importCsaBlob blob

    let encrypt  =
        asProvider 
        >> fun provider -> fun bts -> provider.Encrypt(bts,false) |> AEV


module KeyPairCsp =

    let prefix = Guid.Parse("C0C0C48F-0116-49B5-8417-8EF04B6E469D")

    let asB64String =
        fun (KeyPairCsp blob) -> Convert.ToBase64String(blob)   
        >> KeyPairCspString

    let fromB64String =
        (fun (KeyPairCspString str) -> Convert.FromBase64String  str) 
        >> KeyPairCsp

    let asPublicCsp  =
         (fun (KeyPairCsp blob) -> RSACryptoServiceProvider.importCsaBlob blob)  
         >> (fun provider -> provider.ExportCspBlob(false)) 
         >> PublicCsp

    let asProvider  =
         fun (KeyPairCsp blob) ->  RSACryptoServiceProvider.importCsaBlob blob

    let decrypt  =
        asProvider
        >> (fun provider -> (fun (AEV bts) -> provider.Decrypt(bts,false) |> ADV))

//TEST *****************
let pub,priv = RSACryptoServiceProvider.createRsaKeyPair()


let str = "Detta är ett test" 
let btsString = System.Text.Encoding.UTF8.GetBytes(str);

let encryptor = PublicCsp.encrypt pub
let decryptor = KeyPairCsp.decrypt priv

let aev = encryptor btsString
let (ADV decryptedBts) = decryptor aev
let decryptedStr = System.Text.Encoding.UTF8.GetString(decryptedBts)
//******************* TEST
type Id = Id of Guid

type KeyId = | KeyId of Id

type IdentifierId = | IdentifierId of Guid
open System
module Id =

    let toString  =
        fun (Id id) ->  id.ToString()

    let create =
        Guid.NewGuid >> Id

    let toByteArray =
        toString >> Convert.ToByte

    let toBase64String  =
         toString 
        >> System.Text.Encoding.UTF8.GetBytes
        >> Convert.ToBase64String

    let fromBase64String  =
        Convert.FromBase64String
        >> System.Text.Encoding.UTF8.GetString
        >> Guid.Parse
        >> Id

module KeyId =
    let create =
        Id.create >> KeyId
    let toBase64String =
        fun (KeyId id) -> Id.toBase64String  id
    let fromBase64String =
         Id.fromBase64String >> KeyId

let newId = Id.create()
let idTob64 = Id.toBase64String newId
printfn "%i" idTob64.Length
let idFromb64 = Id.fromBase64String idTob64
//login

//skapa publik och private nyckel med id kopplad till detta och till login alt annat id
//användare behöver nyckel och id -> base64 sträng

    



