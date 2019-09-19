module Api.Todos exposing (delete, get, getAll, new, update)

import Components.Todos as Todos
import Http
import Json.Decode as Decode
import Json.Encode as Enc
import Models.List as TodoList
import Session exposing (Session)


get : Session -> (Result Http.Error Todos.Item -> msg) -> TodoList.Id -> Todos.Id -> Cmd msg
get session toMsg listId todoId =
    Http.get
        { url = Session.makeApiUrl session [ "lists", String.fromInt listId, "todos", Todos.idToString todoId ] [] Nothing
        , expect = Http.expectJson toMsg Todos.itemDecoder
        }


getAll : Session -> (Result Http.Error (List Todos.Item) -> msg) -> TodoList.Id -> Cmd msg
getAll session toMsg todoList =
    Http.get
        { url = Session.makeApiUrl session [ "lists", String.fromInt todoList, "todos" ] [] Nothing
        , expect = Http.expectJson toMsg (Decode.list Todos.itemDecoder)
        }


update : Session -> (Result Http.Error Todos.Item -> msg) -> TodoList.Id -> Todos.Item -> Cmd msg
update session toMsg todoList todoItem =
    Http.request
        { method = "put"
        , headers = []
        , url = Session.makeApiUrl session [ "lists", String.fromInt todoList, "todos" ] [] Nothing
        , body = Http.jsonBody (Todos.encodeItem todoItem)
        , expect = Http.expectJson toMsg Todos.itemDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


delete : Session -> (Result Http.Error (List Todos.Item) -> msg) -> TodoList.Id -> Todos.Id -> Cmd msg
delete session toMsg todoList todo =
    Http.request
        { method = "delete"
        , headers = Session.authHeader session
        , url = Session.makeApiUrl session [ "lists", String.fromInt todoList, "todos", Todos.idToString todo ] [] Nothing
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list Todos.itemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


new : Session -> (Result Http.Error Todos.Item -> msg) -> TodoList.Id -> String -> Cmd msg
new session toMsg todoList text =
    Http.post
        { url = Session.makeApiUrl session [ "lists", String.fromInt todoList, "todos" ] [] Nothing
        , body = Http.jsonBody (Enc.string text)
        , expect = Http.expectJson toMsg Todos.itemDecoder
        }
