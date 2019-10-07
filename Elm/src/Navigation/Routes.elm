module Navigation.Routes exposing (Route(..), locationToRoute, navigateTo, replaceUrl, routeToUrlString)

import Browser.Navigation as Nav
import Flags
import Models.TaskList as TaskList
import Models.Tasks exposing (Filter(..))
import Navigation.AppUrl exposing (BaseUrlPath, buildUrl)
import Session exposing (Session)
import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>), Parser)


type Route
    = Login
    | List TaskList.Id Filter
    | Lists


navigateTo : Session -> Route -> Cmd msg
navigateTo session route =
    let
        url =
            getRouteUrl session route

        key =
            session.navKey
    in
    Nav.pushUrl key url


replaceUrl : Session -> Route -> Cmd msg
replaceUrl session route =
    let
        url =
            getRouteUrl session route

        key =
            session.navKey
    in
    Nav.replaceUrl key url


getRouteUrl : Session -> Route -> String
getRouteUrl session route =
    routeToUrlString session.flags.baseUrlPath route


locationToRoute : BaseUrlPath -> Url -> Maybe Route
locationToRoute baseUrl url =
    UrlP.parse (routeParser baseUrl) url


routeToUrlString : BaseUrlPath -> Route -> String
routeToUrlString baseUrl targetRoute =
    case targetRoute of
        Login ->
            buildUrl baseUrl [ "login" ] [] Nothing

        List id filter ->
            let
                fragment =
                    case filter of
                        Active ->
                            Just "active"

                        Completed ->
                            Just "completed"

                        All ->
                            Nothing
            in
            buildUrl baseUrl [ "list", TaskList.idToString id ] [] fragment

        Lists ->
            buildUrl baseUrl [] [] Nothing


routeParser : BaseUrlPath -> Parser (Route -> a) a
routeParser baseUrl =
    let
        basePart =
            baseUrl
                |> String.split "/"
                |> List.filter (not << String.isEmpty)
                |> List.map UrlP.s
                |> List.foldr (</>) UrlP.top

        filterParser =
            Maybe.map
                (\frag ->
                    case frag of
                        "active" ->
                            Active

                        "completed" ->
                            Completed

                        _ ->
                            All
                )
                >> Maybe.withDefault All
    in
    UrlP.oneOf
        [ UrlP.map List (basePart </> UrlP.s "list" </> UrlP.map TaskList.idFromInt UrlP.int </> UrlP.fragment filterParser)
        , UrlP.map Login (basePart </> UrlP.s "login")
        , UrlP.map Lists basePart
        ]
