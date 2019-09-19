module Models.List exposing (List, ListId, MetaData)

import Todos exposing (Todos)


type alias ListId =
    Int


type alias List =
    { metaData : MetaData
    , todos : Todos
    }


type alias MetaData =
    { name : String
    , id : ListId
    , active : Int
    }
