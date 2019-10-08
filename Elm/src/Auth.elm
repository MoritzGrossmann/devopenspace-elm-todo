module Auth exposing
    ( AuthToken, Authentication(..), ModelWithAuth
    , init, isAuthenticated, isNotQueried, clearAuthentication
    , httpLogin, httpRegister, basicAuthHeader, bearerAuthHeader
    , getUserName
    )

{-| dieses Modul fast Funktionalitäten im Zusammenhang mit der
Authentifizierung/Autorisierung von Benutzern am Backend-API zusammen

Die meisten Methoden erwarten ein _Bearer_ Token (JWT) im 'Authorization'
Header. Dieses Token wird nach dem **Registrieren** oder auf Antwort auf ein
erfolgreiches **Login** vom Server zurückgegeben.

Die App speichert dieses Token im _LocalStroage_, so dass beim Start/Refresh der
App nicht unbedingt ein **Login** erfolgen muss.

**Hinweis:** Wir verwenden die Begriffe _Authorization_/_Authentication_ hier
etwas lax, fast als Synonyme - für das Backend-API ist jeder Authentifizierte
User automatisch berechtigt seine Listen/Tasks zu verwalten - der Benutzer
wird dabei _implizit_ über das Token ermittelt.


# exportierte Typen

@docs AuthToken, Authentication, ModelWithAuth


# nützliche Hilfsfunktionen und Eigenschaftsabfragen

@docs init, isAuthenticated, isNotQueried, clearAuthentication


# API Requests

@docs httpLogin, httpRegister, basicAuthHeader, bearerAuthHeader

-}

import Base64
import Flags exposing (Flags)
import Http
import Json.Decode as Json
import Json.Encode as Enc
import Jwt
import Navigation.AppUrl as AppUrl
import String.Interpolate as String
import Url.Builder as Url


{-| einfacher Wrapper um ein JWT Token wie es aus dem LocalStorage oder vom
Backend geliefert wird.

Der Konstruktor wird hier **nicht** exportiert damit gewährleistet ist, dass
dieses Modul volle Kontrolle über die Verwendung dieser Tokens hat

-}
type AuthToken
    = AuthToken String


{-| liegt 'AuthToken' vor - zu Programmstart ist der Zustand auf 'NotQueried'
bis aus dem LocalStorage versucht wurde das Token zu laden
-}
type Authentication
    = NotQueried
    | Authenticated AuthToken
    | NotAuthenticated


{-| einfaches Alias für Records, die ein 'authentication'-Feld enthalten -
der _Session_-Typ der Applikation - verhindert direkte Abhängigkeiten
-}
type alias ModelWithAuth m =
    { m | authentication : Authentication }



-- Hilfsfunktionen


{-| zu Programmstart ist bis zur Abfrage des LocalStorage unklar, ob ein Token
vorliegt

dieser Wert soll das beschreiben

-}
init : Authentication
init =
    NotQueried


{-| ist ein Benutzer laut _Session_ eingeloggt

    isAuthenticated { ..., authentication = Authenticated (AuthToken "...."), ...} == True
    isAuthenticated { ..., authentication = NotAuthenticated, ...} == False
    isAuthenticated { ..., authentication = NotQueried, ...} == False

-}
isAuthenticated : ModelWithAuth m -> Bool
isAuthenticated model =
    case model.authentication of
        Authenticated _ ->
            True

        _ ->
            False


{-| wird noch auf Antwort vom LocalStorage gewartet?

    isNotQueried { ..., authentication = Authenticated (AuthToken "...."), ...} == False
    isNotQueried { ..., authentication = NotAuthenticated, ...} == False
    isNotQueried { ..., authentication = NotQueried, ...} == True

-}
isNotQueried : ModelWithAuth m -> Bool
isNotQueried model =
    case model.authentication of
        NotQueried ->
            True

        _ ->
            False


{-| entfernt die Authentication aus der Session
-}
clearAuthentication : ModelWithAuth m -> ModelWithAuth m
clearAuthentication =
    updateAuthentication NotAuthenticated


updateAuthentication : Authentication -> ModelWithAuth m -> ModelWithAuth m
updateAuthentication newAuth model =
    { model | authentication = newAuth }


setAuthenticated : AuthToken -> ModelWithAuth m -> ModelWithAuth m
setAuthenticated newToken =
    updateAuthentication (Authenticated newToken)



-- API Methoden zum Registrieren / LogiVn


{-| Schickt einen Login-Request ans Backend

Die Reaktion wird ein eine Message übersetzt, die die resultiernde
Session-Änderung durchführen kann (siehe 'watchLocalStorage')

Bitte 'updateLocalStorage' als Reaktion auf den Empfang des Ergebnisses dieses
Kommandos nutzen um den eventuell geänderten Zustand im LocalStorage zu
synchronisieren

Leider erlaubt es Elm nicht 'Cmd's zu verketten, so dass dies hier leider
manuell erfolgen muss.

-}
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
        , url = AppUrl.apiUserLoginUrl flags
        , body = Http.emptyBody
        , expect = Http.expectJson mapJwt Json.string
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Schickt einen Register-Request ans Backend

Die Reaktion wird ein eine Message übersetzt, die die resultiernde
Session-Änderung durchführen kann (siehe 'watchLocalStorage')

Bitte 'updateLocalStorage' als Reaktion auf den Empfang des Ergebnisses dieses
Kommandos nutzen um den eventuell geänderten Zustand im LocalStorage zu
synchronisieren

Leider erlaubt es Elm nicht 'Cmd's zu verketten, so dass dies hier leider
manuell erfolgen muss.

-}
httpRegister : Flags -> (Result Http.Error (ModelWithAuth m -> ModelWithAuth m) -> msg) -> String -> String -> Cmd msg
httpRegister flags toMsg username password =
    let
        mapJwt =
            toMsg << Result.map (AuthToken >> setAuthenticated)
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = AppUrl.apiUserRegisterUrl flags
        , body =
            Http.jsonBody
                (Enc.object
                    [ ( "name", Enc.string username )
                    , ( "password", Enc.string password )
                    ]
                )
        , expect = Http.expectJson mapJwt Json.string
        , timeout = Nothing
        , tracker = Nothing
        }


{-| versucht das Auth-Token aus der Session zu lesen und als 'Authorization'
Header zu übergeben

falls kein Token vorliegt wird kein Header-Wert zurückgegeben

die Liste fasst diese beiden Optionen zusammen und ist einfacher zu benutzen
als ein genauerer 'Maybe' Wert

-}
bearerAuthHeader : ModelWithAuth m -> List Http.Header
bearerAuthHeader model =
    case model.authentication of
        Authenticated (AuthToken bearerValue) ->
            [ Http.header "Authorization" (String.interpolate "Bearer {0}" [ bearerValue ])
            ]

        _ ->
            []


{-| Baut einen BasicAuth-Header aus dem übergebenen Benutzername/Passwort-Paar zusammen
-}
basicAuthHeader : String -> String -> Http.Header
basicAuthHeader username password =
    let
        up =
            String.interpolate "{0}:{1}" [ username, password ]
    in
    Http.header "Authorization" (String.interpolate "Basic {0}" [ Base64.encode up ])


jwtDecoder : Json.Decoder Jwt
jwtDecoder =
    Json.map Jwt
        (Json.field "dat" (Json.map JwtDatPart (Json.field "auName" Json.string)))


type alias Jwt =
    { dat : JwtDatPart
    }


type alias JwtDatPart =
    { auName : String
    }


getUserName : { a | authentication : Authentication } -> Maybe String
getUserName session =
    case session.authentication of
        Authenticated (AuthToken token) ->
            Jwt.decodeToken jwtDecoder token
                |> Result.map (\jwt -> jwt.dat.auName)
                |> Result.toMaybe

        _ ->
            Nothing
