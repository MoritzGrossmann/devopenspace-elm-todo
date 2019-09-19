module Flags exposing (BaseUrlPath, Flags, buildUrl, makeUrl)

import Url.Builder as Url


type alias Flags =
    { baseUrlPath : BaseUrlPath
    }


type alias BaseUrlPath =
    String


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
