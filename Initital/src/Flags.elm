module Flags exposing (BaseUrlPath, Flags, buildUrl, makeApiUrl, makeUrl)

import Url.Builder as Url


type alias BaseUrlPath =
    String

type alias Flags =
    { baseUrlPath : BaseUrlPath
    , apiUrl : BaseUrlPath
    }


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


makeUrl : Flags -> List String -> List Url.QueryParameter -> Maybe String -> String
makeUrl flags path query fragment =
    buildUrl flags.baseUrlPath path query fragment


makeApiUrl : Flags -> List String -> List Url.QueryParameter -> Maybe String -> String
makeApiUrl flags path query fragment =
    buildUrl flags.apiUrl path query fragment
