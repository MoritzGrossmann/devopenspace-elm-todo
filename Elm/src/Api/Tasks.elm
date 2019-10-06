module Api.Tasks exposing (delete, get, getAll, new, update)

import Auth
import Http
import Json.Decode as Decode
import Json.Encode as Enc
import Models.Task as Task exposing (Task)
import Models.TaskList as TaskList
import Models.Tasks as Tasks
import Session exposing (Session)


get : Session -> (Result Http.Error Task -> msg) -> Task.Id -> Cmd msg
get session toMsg taskId =
    Http.request
        { method = "GET"
        , headers = Auth.bearerAuthHeader session
        , url = Session.makeApiUrl session [ "todos", Task.idToString taskId ] [] Nothing
        , expect = Http.expectJson toMsg Task.decoder
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


getAll : Session -> (Result Http.Error (List Task) -> msg) -> TaskList.Id -> Cmd msg
getAll session toMsg listId =
    Http.request
        { method = "GET"
        , headers = Auth.bearerAuthHeader session
        , url = Session.makeApiUrl session [ "list", TaskList.idToString listId, "todos" ] [] Nothing
        , expect = Http.expectJson toMsg (Decode.list Task.decoder)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


update : Session -> (Result Http.Error Task -> msg) -> TaskList.Id -> Task -> Cmd msg
update session toMsg listId task =
    Http.request
        { method = "put"
        , headers = Auth.bearerAuthHeader session
        , url = Session.makeApiUrl session [ "todos" ] [] Nothing
        , body = Http.jsonBody (Task.encode listId task)
        , expect = Http.expectJson toMsg Task.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


delete : Session -> (Result Http.Error (List Task) -> msg) -> Task.Id -> Cmd msg
delete session toMsg task =
    Http.request
        { method = "delete"
        , headers = Auth.bearerAuthHeader session
        , url = Session.makeApiUrl session [ "todos", Task.idToString task ] [] Nothing
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list Task.decoder)
        , timeout = Nothing
        , tracker = Nothing
        }


new : Session -> (Result Http.Error Task -> msg) -> TaskList.Id -> String -> Cmd msg
new session toMsg listId text =
    Http.request
        { method = "POST"
        , headers = Auth.bearerAuthHeader session
        , url = Session.makeApiUrl session [ "list", TaskList.idToString listId, "todos" ] [] Nothing
        , body = Http.jsonBody (Enc.string text)
        , expect = Http.expectJson toMsg Task.decoder
        , timeout = Nothing
        , tracker = Nothing
        }
