module Navigation.AppUrl exposing (..)

import Url.Builder as Url


type alias BaseUrlPath =
    Url


type alias Url =
    String


type alias Flags f =
    { f | apiUrl : BaseUrlPath, baseUrlPath : BaseUrlPath }



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
