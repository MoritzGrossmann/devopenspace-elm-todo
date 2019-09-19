module Models.User exposing (Id, Info, decodeId, decodeInfo, encodeId)

import Json.Decode as Json exposing (Decoder)
import Json.Encode as Enc exposing (Value)


type alias Id =
    String


type alias Info =
    { name : String }


decodeInfo : Decoder Info
decodeInfo =
    Json.map Info (Json.field "benutzerName" Json.string)


decodeId : Decoder Id
decodeId =
    Json.string


encodeId : Id -> Value
encodeId =
    Enc.string
