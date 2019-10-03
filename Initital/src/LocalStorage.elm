port module LocalStorage exposing (StorageKey, authorizationKey, receive, request, store)

import Json.Decode exposing (Value)


type alias StorageKey =
    String


authorizationKey : String
authorizationKey =
    "Authorization"


port store : ( StorageKey, Maybe Value ) -> Cmd msg


port request : StorageKey -> Cmd msg


port receive : (( StorageKey, Maybe String ) -> msg) -> Sub msg