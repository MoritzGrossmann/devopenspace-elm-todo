module Api.Lists exposing (add, all, byId, delete)

import Auth
import Http
import Json.Decode as Json
import Json.Encode as Enc
import Models.TaskList as TaskList exposing (TaskList)
import Navigation.AppUrl as AppUrl
import Session exposing (Session)


all : Session -> (Result Http.Error (List TaskList) -> msg) -> Cmd msg
all session toMsg =
    Http.request
        { method = "GET"
        , headers = Auth.bearerAuthHeader session
        , url = AppUrl.apiListGetAllUrl session.flags
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Json.list TaskList.decoder)
        , timeout = Nothing
        , tracker = Nothing
        }


byId : Session -> (Result Http.Error TaskList -> msg) -> TaskList.Id -> Cmd msg
byId session toMsg listId =
    Http.request
        { method = "GET"
        , headers = Auth.bearerAuthHeader session
        , url = AppUrl.apiListGetByIdUrl session.flags listId
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg TaskList.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


delete : Session -> (Result Http.Error () -> msg) -> TaskList.Id -> Cmd msg
delete session toMsg listId =
    Http.request
        { method = "DELETE"
        , headers = Auth.bearerAuthHeader session
        , url = AppUrl.apiListDeleteByIdUrl session.flags listId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


add : Session -> (Result Http.Error TaskList -> msg) -> String -> Cmd msg
add session toMsg name =
    Http.request
        { method = "POST"
        , headers = Auth.bearerAuthHeader session
        , url = AppUrl.apiListPostNewUrl session.flags
        , body = Http.jsonBody (name |> Enc.string)
        , expect = Http.expectJson toMsg TaskList.decoder
        , timeout = Nothing
        , tracker = Nothing
        }
