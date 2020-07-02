
namespace EncryptServer
open EncryptServer.AssymetricEncryption


type RegisterKeyRingRequest =
    {

        KeyPairCsp: string //passwordencypted option
        PublicCsp: string //base64stringofpublicCp
    }

type RegisterKeyRingResponse =
    {
        //server-public key
        //publicCsp signed by server
        ServerPublicKey: string

    }