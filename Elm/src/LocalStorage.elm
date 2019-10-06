port module LocalStorage exposing (StorageKey, authorizationKey, authorizationValueChanged, receive, request, store)

import Json.Decode exposing (Value)


type alias StorageKey =
    String


authorizationKey : String
authorizationKey =
    "Authorization"


port localStorageChanged : (( StorageKey, Maybe String ) -> msg) -> Sub msg


authorizationValueChanged : msg -> (Maybe String -> msg) -> Sub msg
authorizationValueChanged otherMsg valueChangedMsg =
    let
        decide ( key, newValue ) =
            if key == authorizationKey then
                valueChangedMsg newValue

            else
                otherMsg
    in
    localStorageChanged decide


port store : ( StorageKey, Maybe Value ) -> Cmd msg


port request : StorageKey -> Cmd msg


port receive : (( StorageKey, Maybe String ) -> msg) -> Sub msg
