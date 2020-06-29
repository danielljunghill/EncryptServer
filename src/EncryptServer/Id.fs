namespace EncryptServer
open System

type Id = Id of Guid
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

