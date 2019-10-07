module AppUrl exposing (..)

import Models.Task as Task
import Models.TaskList as TaskList
import Url.Builder as Url


type alias BaseUrlPath =
    Url


type alias Url =
    String


type alias Flags f =
    { f | apiUrl : BaseUrlPath, baseUrlPath : BaseUrlPath }



-- API URLs


apiListGetAllUrl : Flags f -> Url
apiListGetAllUrl flags =
    buildApiUrlFromFlags flags [ "list" ] [] Nothing


apiListGetByIdUrl : Flags f -> TaskList.Id -> Url
apiListGetByIdUrl flags lId =
    buildApiUrlFromFlags flags [ "list", TaskList.idToString lId ] [] Nothing


apiListDeleteByIdUrl : Flags f -> TaskList.Id -> Url
apiListDeleteByIdUrl flags lId =
    buildApiUrlFromFlags flags [ "list", TaskList.idToString lId ] [] Nothing


apiListPostNewUrl : Flags f -> Url
apiListPostNewUrl flags =
    buildApiUrlFromFlags flags [ "list" ] [] Nothing


apiTaskGetByIdUrl : Flags f -> Task.Id -> Url
apiTaskGetByIdUrl flags tId =
    buildApiUrlFromFlags flags [ "todos", Task.idToString tId ] [] Nothing


apiTaskGetAllUrl : Flags f -> TaskList.Id -> Url
apiTaskGetAllUrl flags lId =
    buildApiUrlFromFlags flags [ "list", TaskList.idToString lId, "todos" ] [] Nothing


apiTaskUpdateUrl : Flags f -> Url
apiTaskUpdateUrl flags =
    buildApiUrlFromFlags flags [ "todos" ] [] Nothing


apiTaskDeleteUrl : Flags f -> Task.Id -> Url
apiTaskDeleteUrl flags tId =
    buildApiUrlFromFlags flags [ "todos", Task.idToString tId ] [] Nothing


apiTaskPostNewUrl : Flags f -> TaskList.Id -> Url
apiTaskPostNewUrl flags lId =
    buildApiUrlFromFlags flags [ "list", TaskList.idToString lId, "todos" ] [] Nothing


apiUserLoginUrl : Flags f -> Url
apiUserLoginUrl flags =
    buildApiUrlFromFlags flags [ "user", "login" ] [] Nothing


apiUserRegisterUrl : Flags f -> Url
apiUserRegisterUrl flags =
    buildApiUrlFromFlags flags [ "user", "register" ] [] Nothing



-- Funktionen zum Erstellen / Zusammenbauen von URLs


buildUrl : BaseUrlPath -> List String -> List Url.QueryParameter -> Maybe String -> String
buildUrl baseUrlPath path query fragment =
    if String.startsWith "http" baseUrlPath then
        Url.custom (Url.CrossOrigin baseUrlPath) path query fragment

    else
        let
            baseParts =
                baseUrlPath
                    |> String.split "/"
                    |> List.filter (not << String.isEmpty)

            absolutePath =
                if List.isEmpty baseParts then
                    path

                else
                    baseParts ++ path
        in
        Url.custom Url.Absolute absolutePath query fragment


buildUrlFromFlags : { f | baseUrlPath : BaseUrlPath } -> List String -> List Url.QueryParameter -> Maybe String -> String
buildUrlFromFlags flags path query fragment =
    buildUrl flags.baseUrlPath path query fragment


buildApiUrlFromFlags : { f | apiUrl : BaseUrlPath } -> List String -> List Url.QueryParameter -> Maybe String -> String
buildApiUrlFromFlags flags path query fragment =
    buildUrl flags.apiUrl path query fragment
