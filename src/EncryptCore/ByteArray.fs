namespace EncryptCore
open System
module ByteArray =
    let toString (bts: byte[]) =
        System.Text.Encoding.UTF8.GetString(bts)
    let toBase64String = Convert.ToBase64String
    let fromBase64String = Convert.FromBase64String



    
