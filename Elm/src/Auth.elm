module Auth exposing
    ( AuthToken
    , Authentication(..)
    , ModelWithAuth
    , basicAuthHeader
    , bearerAuthHeader
    , httpLogin
    , init
    , isAuthenticated
    , isNotQueried
    , requestLocalStorageAuth
    , updateLocalStorage
    , watchLocalStorage
    )

import Base64
import Flags exposing (Flags, makeApiUrl)
import Http
import Json.Decode as Json
import Json.Encode as Enc
import LocalStorage
import String.Interpolate as String
import Url.Builder as Url


type AuthToken
    = AuthToken String


type Authentication
    = NotQueried
    | Authenticated AuthToken
    | NotAuthenticated


init : Authentication
init =
    NotQueried


type alias ModelWithAuth m =
    { m | authentication : Authentication }


isAuthenticated : ModelWithAuth m -> Bool
isAuthenticated model =
    case model.authentication of
        Authenticated _ ->
            True

        _ ->
            False


isNotQueried : ModelWithAuth m -> Bool
isNotQueried model =
    case model.authentication of
        NotQueried ->
            True

        _ ->
            False


updateAuthentication : Authentication -> ModelWithAuth m -> ModelWithAuth m
updateAuthentication newAuth model =
    { model | authentication = newAuth }


setAuthenticated : AuthToken -> ModelWithAuth m -> ModelWithAuth m
setAuthenticated newToken =
    updateAuthentication (Authenticated newToken)


clearAuthentication : ModelWithAuth m -> ModelWithAuth m
clearAuthentication =
    updateAuthentication NotAuthenticated


watchLocalStorage : msg -> ((ModelWithAuth m -> ModelWithAuth m) -> msg) -> Sub msg
watchLocalStorage noOpMsg toUpdMsg =
    let
        maybeUpdate =
            Maybe.map (AuthToken >> setAuthenticated >> toUpdMsg) >> Maybe.withDefault (toUpdMsg clearAuthentication)

        onChange =
            LocalStorage.authorizationValueChanged noOpMsg maybeUpdate

        onReceive =
            LocalStorage.authorizationValueReceived noOpMsg maybeUpdate
    in
    Sub.batch [ onChange, onReceive ]


requestLocalStorageAuth : Cmd msg
requestLocalStorageAuth =
    LocalStorage.request LocalStorage.authorizationKey


updateLocalStorage : Authentication -> Cmd msg
updateLocalStorage auth =
    case auth of
        NotQueried ->
            Cmd.none

        NotAuthenticated ->
            resetStoreToken

        Authenticated authToken ->
            setStoreToken authToken


setStoreToken : AuthToken -> Cmd msg
setStoreToken (AuthToken token) =
    LocalStorage.store ( LocalStorage.authorizationKey, Just (Enc.string token) )


resetStoreToken : Cmd msg
resetStoreToken =
    LocalStorage.store ( LocalStorage.authorizationKey, Nothing )



-- | Note you should use 'updateLocalStorage' after receiving an anwer to this to make sure the state is stored correctly
-- sadly I cannot do this for you as I cannot chain Cmds in Elm right now


httpLogin : Flags -> (Result Http.Error (ModelWithAuth m -> ModelWithAuth m) -> msg) -> String -> String -> Cmd msg
httpLogin flags toMsg username password =
    let
        mapJwt =
            toMsg << Result.map (AuthToken >> setAuthenticated)
    in
    Http.request
        { method = "GET"
        , headers =
            [ basicAuthHeader username password
            ]
        , url = makeApiUrl flags [ "user", "login" ] [] Nothing
        , body = Http.emptyBody
        , expect = Http.expectJson mapJwt Json.string
        , timeout = Nothing
        , tracker = Nothing
        }


bearerAuthHeader : ModelWithAuth m -> List Http.Header
bearerAuthHeader model =
    case model.authentication of
        Authenticated (AuthToken bearerValue) ->
            [ Http.header "Authorization" (String.interpolate "Bearer {0}" [ bearerValue ])
            ]

        _ ->
            []


basicAuthHeader : String -> String -> Http.Header
basicAuthHeader username password =
    let
        up =
            String.interpolate "{0}:{1}" [ username, password ]
    in
    Http.header "Authorization" (String.interpolate "Basic {0}" [ Base64.encode up ])
