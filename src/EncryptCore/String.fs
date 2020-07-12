namespace EncryptCore
open System

module String =
    let toByteArray (s: string)=
        System.Text.Encoding.UTF8.GetBytes(s)

type Base64String = private | Base64String of string
module Base64String =
    let fromByteArray = Convert.ToBase64String >> Base64String
    let toByteArray (Base64String str) = Convert.FromBase64String str


//type Base64String = private |Base64String of string

////module Base64String =
////    let fromByteArray  = 
////         Convert.ToBase64String >> Base64String    
////    let toByteArray (Base64String str) = 
////        Convert.FromBase64String str

//module Int =
//    let toByteArray (i:int) =
//        BitConverter.GetBytes(i)
//    let toBase64String =
//        toByteArray >> Convert.ToBase64String
//    let length =
//        let s = toBase64String 1
//        s.Length
//    let fromBase64String  =
//        Convert.FromBase64String
//        >> (fun bts -> BitConverter.ToInt32(bts,0))

//    let parseLengthFromBase64String (s:string) =
//        s.Substring(0,length) |> fromBase64String,
//        s.Substring(length,s.Length - length)
        
        

//type ByteMap<'T> = | ByteMap of ('T -> byte[])

//module Base64String =
//    let create<'T> (v: 'T) (bm: ByteMap<'T>)  =
//       let (ByteMap map) = bm
//       let str = v |> map |> Convert.ToBase64String
//       let prefix = Int.toBase64String str.Length
//       sprintf "%s%s" prefix str |> Base64String


//    let toByteArray (Base64String s) =
//        Convert.FromBase64String(s)

//    let parse s =
        
    
//    str:string) = 
//        let len = str
   
  

      
    
    