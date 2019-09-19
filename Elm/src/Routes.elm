module Routes exposing (Route(..), locationToRoute, routeToUrlString)

import Flags exposing (BaseUrlPath, buildUrl)
import Todos exposing (Filter(..))
import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>), Parser)


type Route
    = Login
    | List Int Filter
    | Lists


locationToRoute : BaseUrlPath -> Url -> Maybe Route
locationToRoute baseUrl url =
    UrlP.parse (route baseUrl) url


routeToUrlString : BaseUrlPath -> Route -> String
routeToUrlString baseUrl targetRoute =
    case targetRoute of
        Login ->
            buildUrl baseUrl [ "login" ] [] Nothing

        List id filter ->
            let
                fragment =
                    case filter of
                        All ->
                            Nothing

                        Active ->
                            Just "active"

                        Completed ->
                            Just "completed"
            in
            buildUrl baseUrl [ "lists", id |> String.fromInt ] [] fragment

        Lists ->
            buildUrl baseUrl [ "lists" ] [] Nothing


route : BaseUrlPath -> Parser (Route -> a) a
route baseUrl =
    let
        basePart =
            baseUrl
                |> String.split "/"
                |> List.filter (not << String.isEmpty)
                |> List.map UrlP.s
                |> List.foldr (</>) UrlP.top
    in
    UrlP.oneOf
        [ UrlP.map Login (basePart </> UrlP.s "login")
        ]
