module Api.Session exposing (post)

import Base64
import Http
import Json.Encode as Enc
import Session exposing (Session)
import String.Interpolate as String


post : Session -> (Result Http.Error String -> msg) -> String -> String -> Cmd msg
post session toMsg username password =
    Http.request
        { method = "GET"
        , headers =
            [ basicAuthHeader username password
            , Http.header "Content-Type" "application/json"
            , Http.header "Access-Control-Request-Method" "GET"
            , Http.header "Origin" "127.0.0.1"
            ]
        , url = Session.makeApiUrl session [ "user", "login" ] [] Nothing
        , body = Http.emptyBody
        , expect = Http.expectString toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


basicAuthHeader : String -> String -> Http.Header
basicAuthHeader username password =
    let
        up =
            String.interpolate "{0}:{1}" [ username, password ]
    in
    Http.header "Authorization" (String.interpolate "Basic {0}" [ Base64.encode up ])
