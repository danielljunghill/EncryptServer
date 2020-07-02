namespace EncryptCore

module String =
    let toByteArray (s: string)=
        System.Text.Encoding.UTF8.GetBytes(s)