module Navigation.Routes exposing (Route(..), locationToRoute, navigateTo, replaceUrl, routeToUrlString)

import Browser.Navigation as Nav
import Flags
import Navigation.AppUrl exposing (BaseUrlPath, buildUrl)
import Session exposing (Session)
import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>), Parser)


type Route
    = Login
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
    in
    UrlP.oneOf
        [ UrlP.map Login (basePart </> UrlP.s "login")
        , UrlP.map Lists basePart
        ]
