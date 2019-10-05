module Api.Todos exposing (delete, get, getAll, new, update)

import Components.Todos as Todos
import Http
import Json.Decode as Decode
import Json.Encode as Enc
import Models.List as TList
import Session exposing (Session)


get : Session -> (Result Http.Error Todos.Item -> msg) -> Todos.Id -> Cmd msg
get session toMsg todoId =
    Http.request
        { method = "GET"
        , headers = Session.authHeader session
        , url = Session.makeApiUrl session [ "todos", Todos.idToString todoId ] [] Nothing
        , expect = Http.expectJson toMsg Todos.itemDecoder
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


getAll : Session -> (Result Http.Error (List Todos.Item) -> msg) -> TList.Id -> Cmd msg
getAll session toMsg listId =
    Http.request
        { method = "GET"
        , headers = Session.authHeader session
        , url = Session.makeApiUrl session [ "list", TList.idToString listId, "todos" ] [] Nothing
        , expect = Http.expectJson toMsg (Decode.list Todos.itemDecoder)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


update : Session -> (Result Http.Error Todos.Item -> msg) -> TList.Id -> Todos.Item -> Cmd msg
update session toMsg listId todoItem =
    Http.request
        { method = "put"
        , headers = Session.authHeader session
        , url = Session.makeApiUrl session [ "todos" ] [] Nothing
        , body = Http.jsonBody (Todos.encodeItem listId todoItem)
        , expect = Http.expectJson toMsg Todos.itemDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


delete : Session -> (Result Http.Error (List Todos.Item) -> msg) -> Todos.Id -> Cmd msg
delete session toMsg todo =
    Http.request
        { method = "delete"
        , headers = Session.authHeader session
        , url = Session.makeApiUrl session [ "todos", Todos.idToString todo ] [] Nothing
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list Todos.itemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


new : Session -> (Result Http.Error Todos.Item -> msg) -> TList.Id -> String -> Cmd msg
new session toMsg listId text =
    Http.request
        { method = "POST"
        , headers = Session.authHeader session
        , url = Session.makeApiUrl session [ "list", TList.idToString listId, "todos" ] [] Nothing
        , body = Http.jsonBody (Enc.string text)
        , expect = Http.expectJson toMsg Todos.itemDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
