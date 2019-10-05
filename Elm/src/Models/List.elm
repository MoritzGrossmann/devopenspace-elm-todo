module Models.List exposing (Id, List, MetaData, decodeMetaData, encodeId, idDecoder, idFromInt, idToInt, idToString)

import Json.Decode as Json exposing (Decoder)
import Json.Encode as Enc exposing (Value)


type Id
    = Id Int


idDecoder : Decoder Id
idDecoder =
    Json.map Id Json.int


encodeId : Id -> Value
encodeId (Id id) =
    Enc.int id


idToString : Id -> String
idToString (Id id) =
    String.fromInt id


idToInt : Id -> Int
idToInt (Id id) =
    id


idFromInt : Int -> Id
idFromInt =
    Id


type alias List =
    { metaData : MetaData
    }


type alias MetaData =
    { name : String
    , id : Id
    , active : Int
    }


decodeMetaData : Json.Decoder MetaData
decodeMetaData =
    Json.map3 MetaData
        (Json.field "name" Json.string)
        (Json.field "id" idDecoder)
        (Json.field "nrActive" Json.int)
