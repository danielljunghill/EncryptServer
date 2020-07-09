namespace EncryptCore
open System

module String =
    let toByteArray (s: string)=
        System.Text.Encoding.UTF8.GetBytes(s)

type Base64String = private |Base64String of string
module Base64String =
 
    let fromByteArray  = 
         Convert.ToBase64String >> Base64String
     
    let toByteArray (Base64String str) = 
        Convert.FromBase64String str

   
   
  

      
    
    