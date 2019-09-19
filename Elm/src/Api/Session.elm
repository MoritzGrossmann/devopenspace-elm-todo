module Api.Session exposing (post)

import Http
import Json.Encode as Enc
import Session exposing (Session)


post : { m | session : Session } -> (Result Http.Error String -> msg) -> String -> String -> Cmd msg
post model toMsg username password =
    let
        json =
            Enc.object
                [ ( "username", Enc.string username )
                , ( "password", Enc.string password )
                ]
    in
    Http.post
        { url = Session.getUrl model [ "login" ] [] Nothing
        , body = Http.jsonBody json
        , expect = Http.expectString toMsg
        }
