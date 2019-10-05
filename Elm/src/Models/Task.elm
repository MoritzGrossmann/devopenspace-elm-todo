module Models.Task exposing (Id, Task, decoder, encode, encodeId, idDecoder, idFromInt, idToInt, idToString)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Models.List as List


type alias Task =
    { id : Id
    , text : String
    , completed : Bool
    }


decoder : Decoder Task
decoder =
    Decode.map3
        Task
        (Decode.field "id" idDecoder)
        (Decode.field "text" Decode.string)
        (Decode.field "finished" Decode.bool)


encode : List.Id -> Task -> Value
encode listId item =
    Encode.object
        [ ( "id", encodeId item.id )
        , ( "text", Encode.string item.text )
        , ( "finished", Encode.bool item.completed )
        , ( "listId", List.encodeId listId )
        ]


type Id
    = Id Int


idDecoder : Decoder Id
idDecoder =
    Decode.map Id Decode.int


encodeId : Id -> Value
encodeId (Id id) =
    Encode.int id


idToString : Id -> String
idToString (Id id) =
    String.fromInt id


idToInt : Id -> Int
idToInt (Id id) =
    id


idFromInt : Int -> Id
idFromInt =
    Id
