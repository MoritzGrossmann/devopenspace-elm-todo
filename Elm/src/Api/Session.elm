module Api.Session exposing (post)

import Http
import Json.Encode as Enc
import Session exposing (Session)


post : Session -> (Result Http.Error String -> msg) -> String -> String -> Cmd msg
post session toMsg username password =
    let
        json =
            Enc.object
                [ ( "username", Enc.string username )
                , ( "password", Enc.string password )
                ]
    in
    Http.post
        { url = Session.makeApiUrl session [ "login" ] [] Nothing
        , body = Http.jsonBody json
        , expect = Http.expectString toMsg
        }
