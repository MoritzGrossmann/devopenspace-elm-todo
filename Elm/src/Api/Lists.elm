module Api.Lists exposing (add, all, byId, delete)

import Http
import Json.Decode as Json
import Json.Encode as Enc
import Models.List as TodoList
import Session exposing (Session)


all : Session -> (Result Http.Error (List TodoList.MetaData) -> msg) -> Cmd msg
all session toMsg =
    Http.request
        { method = "GET"
        , headers = Session.authHeader session
        , url = Session.makeApiUrl session [ "list" ] [] Nothing
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Json.list TodoList.decodeMetaData)
        , timeout = Nothing
        , tracker = Nothing
        }


byId : Session -> (Result Http.Error TodoList.MetaData -> msg) -> TodoList.Id -> Cmd msg
byId session toMsg listId =
    Http.request
        { method = "GET"
        , headers = Session.authHeader session
        , url = Session.makeApiUrl session [ "list", listId |> String.fromInt ] [] Nothing
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg TodoList.decodeMetaData
        , timeout = Nothing
        , tracker = Nothing
        }


delete : Session -> (Result Http.Error () -> msg) -> TodoList.Id -> Cmd msg
delete session toMsg listId =
    Http.request
        { method = "DELETE"
        , headers = Session.authHeader session
        , url = Session.makeApiUrl session [ "list", listId |> String.fromInt ] [] Nothing
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


add : Session -> (Result Http.Error TodoList.MetaData -> msg) -> String -> Cmd msg
add session toMsg name =
    Http.request
        { method = "POST"
        , headers = Session.authHeader session
        , url = Session.makeApiUrl session [ "list" ] [] Nothing
        , body = Http.jsonBody (name |> Enc.string)
        , expect = Http.expectJson toMsg TodoList.decodeMetaData
        , timeout = Nothing
        , tracker = Nothing
        }
