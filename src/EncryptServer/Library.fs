namespace EncryptServer
open Newtonsoft.Json

module Test =
    type key = 
        {
            key: byte[]
            identity: string
        }

    let k = { key = System.Text.Encoding.UTF8.GetBytes("asdasdasd"); identity = "lknadlkdönakd"}

    let json = JsonConvert.SerializeObject(k)
    let kb = JsonConvert.DeserializeObject<key>(json)
