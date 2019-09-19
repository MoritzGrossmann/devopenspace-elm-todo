module Models.List exposing (Id, List, MetaData, decodeMetaData)

import Components.Todos exposing (Todos)
import Json.Decode as Json


type alias Id =
    Int


type alias List =
    { metaData : MetaData
    , todos : Todos
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
        (Json.field "id" Json.int)
        (Json.field "active" Json.int)
