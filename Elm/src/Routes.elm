module Routes exposing (Route(..), locationToRoute, routeToUrlString)

import Components.Todos exposing (Filter(..))
import Flags exposing (BaseUrlPath, buildUrl)
import Models.List as List
import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>), Parser)


type Route
    = Login
    | List List.Id Filter
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
                            Just "all"

                        Active ->
                            Just "active"

                        Completed ->
                            Just "completed"
            in
            buildUrl baseUrl [ "lists", List.idToString id ] [] fragment

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

        filterParser frag =
            frag
                |> Maybe.map
                    (\f ->
                        case f of
                            "active" ->
                                Active

                            "completed" ->
                                Completed

                            _ ->
                                All
                    )
                |> Maybe.withDefault All
    in
    UrlP.oneOf
        [ UrlP.map List (basePart </> UrlP.s "lists" </> UrlP.map List.idFromInt UrlP.int </> UrlP.fragment filterParser)
        , UrlP.map Lists (basePart </> UrlP.s "lists")
        , UrlP.map Login (basePart </> UrlP.s "login")
        ]
